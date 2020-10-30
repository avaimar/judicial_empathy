

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
cast.senm <- function(dat, ms.arg, two.outcomes=FALSE) {
    ms <- as.vector(ms.arg)

    y <- dat$y[!is.na(ms)]
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


# Matching function
perform_matching <- function(
    match_id,
    dmatrix,
    variables,
    caliper = 0,
    match_ratio = 1,
    exact_variables = NULL,
    almost_exact_variables = NULL,
    fine_balance_variables = NULL
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
    #.....................................................................
    # Generate distance matrix
    if (dmatrix == 'MD') {
        DM <- smahal(z = data_judges$z, 
                     X = data_judges[, variables, with = FALSE])
        
    } else {
        stop('Pending implementation')
    }
    
    # Add caliper if selected
    if (caliper > 0) {
        DM <- addcaliper(dmat = DM, z = data_judges$z, p = data_judges$pscore,
                         caliper = caliper)
    }
  
    # Add exact matching
    if (!missing(exact_variables)) {
        for (v in exact_variables) {
            DM <- addalmostexact(dmat = DM, z = data_judges$z, 
                           f = as.vector(data_judges[[v]]), mult = 100000) 
        }
    }
  
    # Add almost exact matching
    if (!missing(almost_exact_variables)) {
        for (v in almost_exact_variables) {
            DM <- addalmostexact(dmat = DM, z = data_judges$z, 
                                 f = as.vector(data_judges[[v]]), mult = 3) 
        }
    }
  
    # Add fine covariate balance
    ## gives following error:
    ## "Error in match_on.numeric(x, within = within, z = z, ...) : 
    ## You must supply a treatment indicator, 'z', when using the numeric match_on method."
    if (!missing(fine_balance_variables)) {
         DM <- as.vector(rcbalance(DM, fb.list = fine_balance_variables, 
                         treated.info = data_judges[data_judges$z==1,], 
                         control.info = data_judges[data_judges$z==0,], 
                         exclude.treated=TRUE)$matches)
    }
    
    # Generate match
    if (match_ratio == 1) {
        gen_match <- pairmatch(x = DM, data=data_judges)
    } else {
        ## set max control instead of min controls. This is not really what we want
        gen_match <- fullmatch(x = DM, max.controls = match_ratio,
                               data=data_judges)
    }
    
    match_summary <- summarize.match(as.data.frame(data_judges), gen_match, 
                                     ps.name = 'pscore')
    
    # Generate plot
    gen_formula = as.formula(
        paste0('z ~', paste(variables, collapse =  '+'),
               '+ pscore + strata(gen_match) - 1'))
    
    png(paste0(output, match_id))
    plot(xBalance(gen_formula, data = data_judges))
    dev.off()
}
