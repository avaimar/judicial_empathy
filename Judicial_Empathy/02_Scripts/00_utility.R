# Helper functions

## puts the results of a pair match in a nice form
summarize.match <- function(dat, ms, ps.name="prop", keep.mset=FALSE) {
    adat <- dat
    adat$mset <- ms
    adat <- adat[!is.na(adat$mset),]
    adat.treat <- adat[adat$z==1, ]
    adat.ctrl <- adat[adat$z==0, ]

    adat.m <- merge(adat.treat, adat.ctrl, by="mset", suffixes=c(".1", ".0"))

    if(!keep.mset) {
        adat.m <- adat.m[, -which(names(adat.m) %in% c("z.1", "z.0", "mset"))]
    } else {
        adat.m <- adat.m[, -which(names(adat.m) %in% c("z.1", "z.0"))]        
    }
    adat.m <- adat.m[, sort(names(adat.m), index.return=TRUE)$ix]
    
    p0.name <- paste0(ps.name,".", 0)
    p1.name <- paste0(ps.name,".",1)

    adat.m.tmp.1 <- adat.m[, -which(names(adat.m) %in% c(p0.name, p1.name))]
    adat.m.tmp.2 <- adat.m[, c(p0.name, p1.name)]

    adat.m <- cbind(adat.m.tmp.1, adat.m.tmp.2)
    
    return(adat.m)
}

## preprocesses the results of pair matching for an analysis
## using `senm'.
cast.senm <- function(dat, ms.arg, two.outcomes=FALSE, y_col) {
    ms <- as.vector(ms.arg)

    #y <- dat$y[!is.na(ms)]
    #y = dat$progressive.vote[!is.na(ms)]
    #y = dat$avg_resid_rank
    eval(parse(text = paste0('y = dat$', y_col, '[!is.na(ms)]')))
    
    mset <- ms[!is.na(ms)]
    z <- dat$z[!is.na(ms)]
    
    dico.names <- unique(mset)
    dico <- seq(length(dico.names))
    names(dico) <- dico.names
    mset <- as.integer(dico[mset])

    if(two.outcomes==FALSE) {
        return(list(y=y, mset=mset, z=z))
    } else {
        y2 <- dat$y2[!is.na(ms)]
        return(list(y=y, y2=y2, mset=mset, z=z))
    }
}


ms.transform <- function(dat.arg, ms.rcbal) {
  ctrl <- seq(sum(dat.arg$z==0))
  matched.ctrl <- ms.rcbal
  unmatched.ctrl <- setdiff(ctrl,ms.rcbal)
  
  dat.tmp <- dat.arg
  dat.tmp$foo <- NA
  dat.tmp$foo[dat.tmp$z==1] <- matched.ctrl
  dat.tmp$foo[dat.tmp$z==0][matched.ctrl] <- matched.ctrl
  
  return(dat.tmp$foo)    
}

ms.transform.adj <- function(dat.arg, ms.rcbal) {
  # Note: This function has been modified to allow for the case
  # in which we have more treatment than control units, and so
  # each control unit is matched to one treatment unit, and some
  # treatment units are dropped.
  
  matched.ctrl <- ms.rcbal
  no_treated_units <- sum(dat.arg$z == 1)
  
  # Convert matching to a manageable data frame
  ms.rcbal <- data.frame(i = rownames(ms.rcbal), c = ms.rcbal)
  
  # Expand matching vector to all treatment units
  full.ms.rcbal <- data.table(expand.grid(i = factor(1:no_treated_units)))
  full.ms.rcbal <- merge(full.ms.rcbal, 
                         ms.rcbal, by = 'i', all.x = TRUE)
  
  dat.tmp <- dat.arg
  dat.tmp$foo <- NA
  
  # Note: Because we have less T than C units, we can't simply
  # apply the matched vector to treatment units, because some should
  # be NA as they could not be matched.
  dat.tmp$foo[dat.tmp$z==1] <- full.ms.rcbal$X1
  dat.tmp$foo[dat.tmp$z==0][matched.ctrl] <- matched.ctrl
  
  return(dat.tmp$foo)    
}


# Matching function
perform_matching <- function(
    match_id,
    dmatrix,
    variables,
    caliper = 0,
    match_ratio = 1,
    exact_variables = NULL,
    almost_exact_variables = NULL,
    fine_balance_variables = NULL,
    data,
    export = FALSE
) {
    #......................................................................
    # perform_matching generates a distance matrix, matched pairs and covariate
    # imbalance plot according to the user's matching specifications:
    #    - match_id : a string for the plot's file name to identify the match
    #    - dmatrix : a string from the following options {'MD' : Robust Mahalanobis}
    #    - variables: a character vector indicating the variables to match on for
    #      Mahalanobis distance
    #    - caliper : if 0, does not include caliper. Any value above zero includes
    #      a caliper in the distance matrix
    #    - match_ratio: an integer specifying the number of units to match
    #    - exact_variables: character vector of variables to exact match on
    #    - almost_exact_variables: character vector of variables to almost exact match on
    #    - fine_balance_variables: character vector of variables to fine balance on
    #    - data: data.table including the data for matching
    #    - export: Boolean indicating whether to export the data including the
    #      generated match
    #.....................................................................
  
  # Gather number of units
  n_control <- sum(data$z == 0)
  n_treated <- sum(data$z == 1)
  
  # Generate distance matrix
  if (dmatrix == 'MD') {
      DM <- smahal(z = data$z, 
                   X = data[, variables, with = FALSE])
      
  } else {
      stop('Pending implementation')
  }
  
  # Add caliper if selected
  if (caliper > 0) {
      DM <- addcaliper(dmat = DM, z = data$z, p = data$pscore,
                       caliper = caliper)
  }

  # Add exact matching
  if (!missing(exact_variables)) {
      for (v in exact_variables) {
          DM <- addalmostexact(dmat = DM, z = data$z, 
                         f = as.vector(data[[v]]), mult = 100000) 
      }
  }

  # Add almost exact matching
  if (!missing(almost_exact_variables)) {
      for (v in almost_exact_variables) {
          DM <- addalmostexact(dmat = DM, z = data$z, 
                               f = as.vector(data[[v]]), mult = 3) 
      }
  }

  # Add fine covariate balance
  if (!missing(fine_balance_variables)) {
    rc_match <- rcbalance(DM, fb.list = list(fine_balance_variables), 
                          treated.info = data[z==1, variables, with=FALSE], 
                          control.info = data[z==0, variables, with=FALSE], 
                          exclude.treated=TRUE)$matches
    
    
    DM <- ms.transform.adj(dat.arg = as.data.frame(data), 
                           ms.rcbal = rc_match)
    names(DM) <- rownames(data)
  }
  
  # Generate match
  if (match_ratio == 1) {
      gen_match <- pairmatch(x = DM, data=data, z=data$z)
  } else {
      ## set max control instead of min controls. This is not really what we want
      gen_match <- fullmatch(x = DM, max.controls = 1/(match_ratio), 
                             min.controls = 1/(match_ratio), 
                             omit.fraction = ((match_ratio) * n_control - n_treated)/n_treated,
                             data=data)
  }
  
  match_summary <- summarize.match(as.data.frame(data), gen_match, 
                                   ps.name = 'pscore')
  
  # Generate plot
  gen_formula = as.formula(
      paste0('z ~', paste(variables, collapse =  '+'),
             '+ pscore + strata(gen_match) - 1'))
  
  png(paste0(output, match_id), width = 400, height = 600)
  plot(xBalance(gen_formula, data = data))
  dev.off()
  
  # Export match
  if (export != FALSE) {
    new_data <- copy(data)
    new_data <- new_data[, matches := gen_match]
    fwrite(x = new_data, file = export)
  }
}
