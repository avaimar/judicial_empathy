########################################################
## Replication File For: 
## "Identifying Judicial Empathy: 
##  Does Having Daughters Cause Judges to Rule for Women's Issues?"
## American Journal of Political Science
## Adam Glynn (aglynn@fas.harvard.edu) (2014)
## Maya Sen (msen@post.harvard.edu)

## June 2014

########################################################

## loading necessary packages

library(Zelig)
library("ZeligChoice")
library(xtable)		
library(reshape)
library(apsrtable)
library(stargazer)
library(rms)

sim <- Zelig:::sim

cl   <- function(dat,fm, cluster){
           require(sandwich, quietly = TRUE)
           require(lmtest, quietly = TRUE)
           M <- length(unique(cluster))
           N <- length(cluster)
           K <- fm$rank
           dfc <- (M/(M-1))*((N-1)/(N-K))
           uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
           vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
           coeftest(fm, vcovCL) }
           
           
           
########################################################
## Loading the Data
########################################################

## This loads three data sets
## 1. Gender cases
## 2. Cleaned by-judge data
## 3. Non-gender cases, downloaded from Kuersten and Haire's website
## (Commented out; their data are available from their website)

# ## Gender cases only
 women.cases <- read.csv("glynn_sen_daughters_by_case.csv", stringsAsFactors = FALSE) 

# ## Removing male plaintiffs:
 women.cases <- subset(women.cases, femplaintiff == 1)

 women.cases <- subset(women.cases, area == "employment" | area == "Title IX" | area == "pregnancy" | area == "abortion" | area == "reproductive rights")

 women.cases$area <- factor(women.cases$area, levels = c("abortion","employment","pregnancy","reproductive rights","Title IX"))

# ## All cases, including non-gender cases (from Ashlyn Kuersten/Susan Haire's coding)
# cases <- read.csv("cases.for.analysis.csv")	

judge.means <- read.csv("glynn_sen_daughters_by_judge.csv", stringsAsFactors = FALSE)

all <- subset(judge.means, girls != "NaN") # subsets judge
								  # data to those for 
								  # whom we have fertility data

######################################################
## Table 1: Number of children and girls
######################################################
aa <- table(all$republican, all$child)
bb <- table(all$republican, all$girls)

## and now for the table:

xtable(aa)
xtable(bb)

########################################################
## Table 2: Judge Demographics
########################################################

dems <- subset(all, republican == 0)
reps <- subset(all, republican == 1)
women <- subset(all, woman == 1)
men <- subset(all, woman == 0)


mean.kids <-  cbind(mean(na.omit(all$child)), 
		mean(na.omit(dems$child)),
		mean(na.omit(reps$child)),
		mean(na.omit(women$child)),
		mean(na.omit(men$child))
	)

mean.girls <- cbind(mean(na.omit(all$girls)), 
		mean(na.omit(dems$girls)),
		mean(na.omit(reps$girls)),
		mean(na.omit(women$girls)),
		mean(na.omit(men$girls))
	)

prop.zero <- cbind(prop.table(table(na.omit(all$child)))[1],
prop.table(table(na.omit(dems$child)))[1],
prop.table(table(na.omit(reps$child)))[1],
prop.table(table(na.omit(women$child)))[1],
prop.table(table(na.omit(men$child)))[1]
	)
	
	
prop.one <- cbind(prop.table(table(na.omit(all$child)))[2],
prop.table(table(na.omit(dems$child)))[2],
prop.table(table(na.omit(reps$child)))[2],
prop.table(table(na.omit(women$child)))[2],
prop.table(table(na.omit(men$child)))[2]
	)
	
prop.two <- cbind(prop.table(table(na.omit(all$child)))[3],
prop.table(table(na.omit(dems$child)))[3],
prop.table(table(na.omit(reps$child)))[3],
prop.table(table(na.omit(women$child)))[3],
prop.table(table(na.omit(men$child)))[3]
	)

prop.three <- cbind(prop.table(table(na.omit(all$child)))[4],
prop.table(table(na.omit(dems$child)))[4],
prop.table(table(na.omit(reps$child)))[4],
prop.table(table(na.omit(women$child)))[4],
prop.table(table(na.omit(men$child)))[4]
	)

prop.four <- cbind(prop.table(table(na.omit(all$child)))[5],
prop.table(table(na.omit(dems$child)))[5],
prop.table(table(na.omit(reps$child)))[5],
prop.table(table(na.omit(women$child)))[5],
prop.table(table(na.omit(men$child)))[5]
	)

prop.five <- cbind(prop.table(table(na.omit(all$child)))[6],
prop.table(table(na.omit(dems$child)))[6],
prop.table(table(na.omit(reps$child)))[6],
prop.table(table(na.omit(women$child)))[6],
prop.table(table(na.omit(men$child)))[6]
	)
	
aa <- table(na.omit(all$child))
	plus6.all <- sum(aa[7:length(aa)])/sum(aa)
	
bb <- table(na.omit(dems$child))
	plus6.dems <- sum(bb[7:length(bb)])/sum(bb)

cc <- table(na.omit(reps$child))
	plus6.reps <- sum(cc[7:length(cc)])/sum(cc)

dd <- table(na.omit(women$child))
	plus6.women <- sum(dd[7:length(dd)])/sum(dd)

ee <- table(na.omit(men$child))
	plus6.men <- sum(ee[7:length(ee)])/sum(ee)
	
prop.six.or.greater <- cbind(plus6.all, plus6.dems, plus6.reps, plus6.women, plus6.men)
	
mean.female <- cbind(mean(na.omit(all$woman)), 
		mean(na.omit(dems$woman)),
		mean(na.omit(reps$woman)),
		mean(na.omit(women$woman)),
		mean(na.omit(men$woman))
	)
	
mean.rep <- cbind(mean(na.omit(all$republican)), 
		mean(na.omit(dems$republican)),
		mean(na.omit(reps$republican)),
		mean(na.omit(women$republican)),
		mean(na.omit(men$republican))
)


mean.white <- cbind(mean(na.omit(all$race == 1)), 
		mean(na.omit(dems$race == 1)),
		mean(na.omit(reps$race == 1)),
		mean(na.omit(women$race == 1)),
		mean(na.omit(men$race == 1))
	)

mean.yearb <- cbind(mean(na.omit(all$yearb)), 
		mean(na.omit(dems$yearb)),
		mean(na.omit(reps$yearb)),
		mean(na.omit(women$yearb)),
		mean(na.omit(men$yearb))
	)

no_judges <- cbind(nrow(all), nrow(dems), nrow(reps), nrow(women), nrow(men))

demographic_table <- rbind(mean.kids, mean.girls, prop.zero, prop.one,
	prop.two, prop.three, prop.four, prop.five, prop.six.or.greater, mean.female, mean.rep, mean.white, mean.yearb, no_judges)

colnames(demographic_table) <- c("All", "Democrats", "Republicans","Women", "Men")
rownames(demographic_table) <- c("Mean No. Children", "Mean No. Girls",
	"Proportion who have 0 children","1 children",
	"2 children", "3 children",
	"4 children", "5 Children", "6 Children or More", "Proportion Female", "Proportion Republican",
	"Proportion White", "Mean Year Born", "N")
xtable(demographic_table, digits = 2, caption = "Demographics of U.S. Court of Appeal Judges who voted on gender-related cases (1996-2002)", label = "t:statsgender", align = "l|ccccc")

########################################################
## Calculating the Weights (Number of Cases)
########################################################

no_cases <- matrix(data = NA, nrow = nrow(judge.means), ncol = 1)
for(i in 1:length(no_cases)){
	no_cases[i] <- nrow(women.cases[which(women.cases$name == judge.means$name[i]),])
	}

judge.means <- cbind(judge.means, no_cases)

## total number of cases we are working with

sum(judge.means$no_cases) 	# should be 2,674 reported in the paper

########################################################
## Calculating the Outcome Var
########################################################

no_liberalvote <- matrix(data = NA, nrow = nrow(judge.means), ncol = 1)
for(i in 1:length(no_liberalvote)){
	stuff <- women.cases[which(women.cases$name == judge.means$name[i]),]
	no_liberalvote[i] <- nrow(subset(stuff, vote == 2 | vote == 3))
	}

lib_vote_share <- no_liberalvote/no_cases

judge.means <- cbind(judge.means, no_liberalvote, lib_vote_share)
judge.means <- subset(judge.means, girls != "NaN")

########################################################
## Subsetting Data to Various Populations (for use later)
########################################################

## just women:
women.means <- subset(judge.means, woman == 1)

## just men:
men.means <- subset(judge.means, woman == 0)

## just republicans:
rep.means <- subset(judge.means, republican == 1)

## just democrats
dem.means <- subset(judge.means, republican == 0)


#######################################################
## Figure 1
########################################################

#pdf(file= "lib_votes.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(density(judge.means$lib_vote_share), 
	xlim = c(-.4,1.4), ylim = c(0,2.5),
	ylab = "", xlab = "Proportion of Cases Decided in a Feminist Direction", 
	yaxt = "n", 
	bty = "n", 
	main = "",
	col = "black", lwd = 2)
lines(density(rep.means$lib_vote_share), 
	col = "firebrick", lwd = 2, lty  = 2)
lines(density(dem.means$lib_vote_share), 
	col = "dodgerblue", lwd = 2, lty  = 3)
abline(v = .5, col = "grey50", lty = 2)
text(x = .5, y = 2.4, "Less Feminist", col = "grey50", pos = 2, cex = 0.9)
text(x = .5, y = 2.4, "More Feminist", col = "grey50", pos = 4, cex = 0.9)
text(x = .25, y = 1.7, "Republicans", pos = 2, cex = 0.9)
text(x = .7, y = 1, "Democrats", pos = 4, cex = 0.9)
text(x = .075, y = .6, "All", pos = 4, cex = 0.9)
#dev.off()

#######################################################
## Table 3: Distribution of Cases
########################################################

gg <- rbind (summary(judge.means$no_cases), summary(dem.means$no_cases), summary(rep.means$no_cases))
rownames(gg) <- c("All Judges", "Democrats", "Republicans")
xtable(gg, caption = "Distribution of the number of gender-related cases heard per judge, 1996-2002.", label = "t:number_cases1")

########################################################
## Table 4: Core Results (WLS)
########################################################

################ Results for all judges

#judge.means <- subset(judge.means, child > 0)

my.out1 <- lm(lib_vote_share ~ as.factor(girls) + as.factor(child), 
	data = judge.means, weights = judge.means$no_cases)

my.out2 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child), 
	data = judge.means, weights = judge.means$no_cases)

my.out3 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3), 
	data = judge.means, weights = judge.means$no_cases)

my.out4 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit.1), 
	data = judge.means, weights = judge.means$no_cases)
		
################ Results for judges between 1 and 4 children
	
my.out5 <- lm(lib_vote_share ~ as.factor(girls) + as.factor(child), 
	data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

my.out6 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child), 
	data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

my.out7 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3), 
	data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

my.out8 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit.1), 
	data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

stargazer(my.out1, my.out2, my.out3, my.out4, my.out5, my.out6, my.out7, my.out8,
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Weighted least squares results, gender cases only. Outcome is proportion of feminist votes.  Models 1--4 are for all judges, while Models 5--8 are for judges with 1--4 children.  (No judge among those with 1--4 children had four girls.) All models include fixed effects for number of children and use weights based on the number of cases heard by each judge.", digits = 2, omit = c("circuit"), covariate.labels = c("1 Girl","2 Girls","3 Girls","4 Girls","5 Girls","At Least 1 Girl",	
	"1 Child","2 Children","3 Children","4 Children","5 Children","6 Children","7 Children","8 Children",
	"9 Children","Republican","Age at Investiture","Catholic","Woman","African American","Hispanic","Constant"))

#######################################################
## Table 5: Core Results (Case-Level)
#######################################################

my.out7 <- zelig(progressive.vote ~ as.factor(girls) + as.factor(child), 
		model = "logit", data = subset(women.cases, child < 5 & child > 0))
summary(my.out7)

my.out8 <- zelig(progressive.vote ~ I(girls > 0) + as.factor(child), 
		model = "logit", data = subset(women.cases, child < 5 & child > 0))
summary(my.out8)

my.out9 <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year), 
		model = "logit", data = subset(women.cases, child < 5 & child > 0))
summary(my.out9)

my.out10 <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + republican 
	+ age + I(religion == 4) + woman + I(race == 2) + I(race == 3) + as.factor(circuit) + as.factor(year) 
	+ as.factor(area), data = subset(women.cases, child < 5 & child > 0), model = "logit")
summary(my.out10)

## including clustered standard errors at the case level (bootstrap)

my.out11 <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + republican 
	+ age + I(religion == 4) + woman + I(race == 2) + I(race == 3) + as.factor(circuit) + as.factor(year) 
	+ as.factor(area), 
	data = subset(women.cases, child < 5 & child > 0), model = "logit")

## Note: Running the ordered logit with polr, in subsetting the data, there are some case areas
## in which there are no observations. Those coefficients are being dropped at POLR gives an
## warning. Nothing to worry.

my.out12 <- zelig(as.factor(vote) ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(area), 
                  model = "ologit", data = subset(women.cases, child < 5 & child > 0))
summary(my.out12)

## Now: Calculating standard errors bootstrapped at the case level:
## Note: This takes about 10 minutes to run. 

## Grab the data for these two models
mod12 <- as.formula("~ progressive.vote + vote + girls +  child + republican +race + age + religion + woman + circuit + year + area + casename")
dat12 <- model.frame(mod12, data = subset(women.cases, child < 5 & child > 0))
dat12$casename <- as.character(dat12$casename)

## Create list of data.frames for each case for easy reference
casenames <- sort(unique(dat12$casename))
caselist <- lapply(casenames, function(x) dat12[which(dat12$casename == x),])
names(caselist) <- casenames

## Running the bootstraps
## Note that not all of the coefficients will be sampled in any given of the 
## bootstraps - this might result in some warnings form polr
## (the ordered logit), but is not a problem for the results

boots <- 1000
b.star11 <- matrix(NA, ncol =length(my.out11$result$coef), nrow = boots)
colnames(b.star11) <- names(my.out11$result$coef)
b.star12 <- matrix(NA, ncol =length(my.out12$result$coef), nrow = boots)
colnames(b.star12) <- names(my.out12$result$coef)
set.seed(1234)
for (b in 1:boots) {
	if ((b%%10) == 0) cat("boot: ", b, "\n")
	clust.sample <- sample(casenames, replace = TRUE)
	c.boot <- do.call(rbind,lapply(clust.sample,function(x) caselist[[x]]))
	out11.star <- glm(progressive.vote ~ I(girls>0) + as.factor(child) + republican 
	+ age + I(religion == 4) + woman + I(race == 2) + I(race == 3) + as.factor(circuit) + as.factor(year) + as.factor(area), data = c.boot, family = binomial("logit"))
    out12.star <- polr(as.factor(vote) ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(area), data = c.boot)	
  b.star11[b,names(out11.star$coef)] <- out11.star$coef
	b.star12[b,names(out12.star$coef)] <- out12.star$coef
		}
bcse11 <- apply(b.star11, 2, sd, na.rm=TRUE)		
bcse12 <- apply(b.star12, 2, sd, na.rm=TRUE)		
#save(list=c("bcse11", "bcse12"), file = "bootstraps.RData")
## End bootstrap code here


## can re-load bootstraped data to avoid having to run previous code
## load("bootstraps.RData")

## Replacing the normal SEs with the bootstrap SEs for the final Table 5
stargazer(my.out7, my.out8, my.out9, my.out10, my.out11, my.out12, 
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Logit and ordered logit results, gender cases only. Outcome is whether judge in a case votes in a feminist direction (Columns 1--5) or in a conservative, moderate, or liberal direction (Column 6).  All models include fixed effects for total number of children and Columns 3--6 include circuit and year fixed effects. Column 5 additionally includes standard errors clustered at the case level", covariate.labels = c("1 Girl","2 Girls","3 Girls","At Least 1 Girl",
	"2 Children","3 Children","4 Children","Republican","Age at Investiture","Catholic","Woman",	
	"African American","Hispanic", "10th Cir", "11th Cir","2nd Cir","3rd Cir",
	"4th Cir","5th Cir","6th Cir","7th Cir","8th Cir","9th Cir","DC","1997","1998","1999","2000","2001","2002",
	"Employment","Pregnancy","Reproduction","Title IX","Constant"),se = list(my.out7 = NULL, my.out8 = NULL, my.out8 = NULL, my.out10 = NULL,my.out11 = bcse11, my.out12 = bcse12), digits = 2)

#######################################################
## Table 6: Comparison for Non-Gender Related Cases
#######################################################

## Commented out:

## cases <- read.csv("cases.for.analysis.csv")

## removing the gendered cases	
cases <- subset(cases, type2 != 231 & type2 != 231 & type2 != 232 & type2 != 233 
				  & type2 != 239 & type2 != 501 & type2 != 503)
	
num.vars <- colnames(cases)[sapply(cases, function(x) is.numeric(x) | is.logical(x))]
case.melt <- melt(cases, id = "judge_name", measure = num.vars)
judge.meansC <- cast(case.melt, judge_name~ variable, mean, na.rm = TRUE)

judge.meansC <- subset(judge.meansC, child < 5 & child > 0)

## calculating the number of cases heard by these judges:

no_cases <- matrix(data = NA, nrow = nrow(judge.meansC), ncol = 1)
for(i in 1:length(no_cases)){
	no_cases[i] <- nrow(cases[which(as.character(cases$judge_name) == as.character(judge.meansC$judge_name[i])),])
	}

judge.meansC <- cbind(judge.meansC, no_cases)
judge.meansC$circuit[judge.meansC$judge_name == "O'Brien, Terrence"] <- 10
judge.meansC$circuit[judge.meansC$judge_name == "Riley, William"] <- 8
judge.meansC$circuit[judge.meansC$judge_name == "Gregory, Roger"] <- 4
judge.meansC$circuit[judge.meansC$judge_name == "King, Robert"] <- 4
judge.meansC$circuit[judge.meansC$judge_name == "Traxler, William"] <- 4
judge.meansC$circuit[judge.meansC$judge_name == "Gibbons, Julia"] <- 6
judge.meansC$circuit[judge.meansC$judge_name == "Melloy, Michael"] <- 8
judge.meansC$circuit[judge.meansC$judge_name == "Berzon, Marsha"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Bye, Kermit"] <- 8
judge.meansC$circuit[judge.meansC$judge_name == "Clement, Edith"] <- 5
judge.meansC$circuit[judge.meansC$judge_name == "Cole, Ransey"] <- 6
judge.meansC$circuit[judge.meansC$judge_name == "Fisher, Raymond"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Fuentes, Julio"] <- 3
judge.meansC$circuit[judge.meansC$judge_name == "Garland, Merrick"] <- 12
judge.meansC$circuit[judge.meansC$judge_name == "Gilman, Ronald"] <- 6
judge.meansC$circuit[judge.meansC$judge_name == "Gould, Ronald"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Graber, Susan"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Hartz, Harris"] <- 10
judge.meansC$circuit[judge.meansC$judge_name == "Hill, James C."] <- 5
judge.meansC$circuit[judge.meansC$judge_name == "Howard, Jeffrey"] <- 1
judge.meansC$circuit[judge.meansC$judge_name == "Hull,  Frank"] <- 11
judge.meansC$circuit[judge.meansC$judge_name == "Kelly, John"] <- 8
judge.meansC$circuit[judge.meansC$judge_name == "King, Carolyn Dineen"] <- 5
judge.meansC$circuit[judge.meansC$judge_name == "Lewis, Timothy K."] <- 3
judge.meansC$circuit[judge.meansC$judge_name == "Lipez, Kermit"] <- 1
judge.meansC$circuit[judge.meansC$judge_name == "Marcus, Stanley"] <- 11
judge.meansC$circuit[judge.meansC$judge_name == "Paez, Richard"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Parker, Fred Irving"] <- 2
judge.meansC$circuit[judge.meansC$judge_name == "Pooler, Rosemary"] <- 2
judge.meansC$circuit[judge.meansC$judge_name == "Porfilio, John"] <- 10
judge.meansC$circuit[judge.meansC$judge_name == "Rendell, Marjorie"] <- 3
judge.meansC$circuit[judge.meansC$judge_name == "Silverman, Barry"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Straub,  Chester"] <- 2
judge.meansC$circuit[judge.meansC$judge_name == "Thomas, Sidney"] <- 9
judge.meansC$circuit[judge.meansC$judge_name == "Tjoflat, Gerald B."] <- 11
judge.meansC$circuit[judge.meansC$judge_name == "Williams, Ann"] <- 7
judge.meansC$circuit[judge.meansC$judge_name == "Wilson, Charles"] <- 12


no_liberalvote <- matrix(data = NA, nrow = nrow(judge.meansC), ncol = 1)
for(i in 1:length(no_liberalvote)){
	stuff <- cases[which(as.character(cases$judge_name) == as.character(judge.meansC$judge_name[i])),]
	no_liberalvote[i] <- nrow(subset(stuff, vote == 2 | vote == 3))
	}

#judge.means <- subset(judge.means, no_cases != 0)

lib_vote_share <- no_liberalvote/no_cases

judge.meansC <- cbind(judge.meansC, no_liberalvote, lib_vote_share)

judge.meansC <- subset(judge.meansC, girls != "NaN")

my.out1c <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = judge.meansC, weights = judge.meansC$no_cases)
summary(my.out1c)

my.out2c <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3), 
	data = judge.meansC, weights = judge.meansC$no_cases)
summary(my.out2c)

my.out3c <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit), 
	data = judge.meansC, weights = judge.meansC$no_cases)
summary(my.out3c)
	
## and then at the case level

cases$progressive.vote <- as.numeric(I(cases$vote > 1))

my.out4c <- zelig(progressive.vote ~ I(girls > 0) + as.factor(child) + as.factor(circuit), 
		model = "logit", data = subset(cases, child < 5 & child > 0))
summary(my.out4c)

my.out5c <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3), 
		model = "logit", data = subset(cases, child < 5 & child > 0))
summary(my.out5c)

my.out6c <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(type), 
		model = "logit", data = subset(cases, child < 5 & child > 0))
summary(my.out6c)


## Including clustered standard errors at the case level (bootstrap)
## Note: Takes about 20 minutes to run

my.out7c <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + party 
                  + age + I(religion == 4) + gender + I(race == 2) + I(race == 3)+ as.factor(circuit) + as.factor(year) 
                  + as.factor(type), 
                  data = subset(cases, child < 5 & child > 0), model = "logit")
                  
my.out8c <- zelig(as.factor(vote) ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(type), 
                  model = "ologit", data = subset(cases, child < 5 & child > 0))


 mod12 <- as.formula("~ progressive.vote +vote+ girls +  child+ party +race + age + religion + gender + circuit + year + type + case")
 dat12 <- model.frame(mod12, data = subset(cases, child < 5 & child > 0))
 dat12$casename <- as.character(dat12$case)
 
 ## create list of data.frames for each case for easy reference
 casenames <- sort(unique(dat12$casename))
 caselist <- lapply(casenames, function(x) dat12[which(dat12$casename == x),])
 names(caselist) <- casenames
 
## Running the bootstraps
## Note that not all of the coefficients will be sampled in any given of the 
## bootstraps - this might result in some warnings form polr
## (the ordered logit), but is not a problem for the results

boots <- 1000
b.star11 <- matrix(NA, ncol =length(my.out7c$result$coef), nrow = boots)
colnames(b.star11) <- names(my.out7c$result$coef)
b.star12 <- matrix(NA, ncol =length(my.out8c$result$coef), nrow = boots)
colnames(b.star12) <- names(my.out8c$result$coef)
set.seed(1234)
for (b in 1:boots) {
	if ((b%%10) == 0) cat("boot: ", b, "\n")
	clust.sample <- sample(casenames, replace = TRUE)
	c.boot <- do.call(rbind,lapply(clust.sample,function(x) caselist[[x]]))
	out11.star <- glm(progressive.vote ~ I(girls>0) + as.factor(child) + party 
	+ age + I(religion == 4) + gender + I(race == 2) + I(race == 3)+ as.factor(circuit) + as.factor(year) 
	+ as.factor(type), data = c.boot, family = binomial("logit"))
    out12.star <- polr(as.factor(vote) ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year) + as.factor(type), data = c.boot)	
  b.star11[b,names(out11.star$coef)] <- out11.star$coef
	b.star12[b,names(out12.star$coef)] <- out12.star$coef
		}
bcse11c <- apply(b.star11, 2, sd, na.rm=TRUE)		
bcse12c <- apply(b.star12, 2, sd, na.rm=TRUE)		
#save(list=c("bcse11c", "bcse12c"), file = "bootstraps-nongender.RData")
# End bootstrap code here
load("bootstraps-nongender.RData")


## Table 6
stargazer(my.out1c, my.out2c, my.out3c, my.out4c, my.out5c, my.out6c, my.out7c, my.out8c,
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Logit and ordered logit results. Outcome is judge's overall liberal voting track record (Models 1--3), whether he or she voted in a liberal direction in an individual case (Models 4--7), or whether he or she voted in a conservative, moderate, or liberal direction in an individual case (Column 8).  All models include fixed effects for total number of children and Columns 3,4,6, and 7 include circuit fixed effects. Column 8 additionally includes standard errors clustered at the case level.", covariate.labels = c("At Least 1 Girl","2 Children","3 Children","4 Children","Republican","Age at Investiture","Catholic","Woman","African American","Hispanic",
	"2nd Cir","3rd Cir","4th Cir","5th Cir","6th Cir","7th Cir","8th Cir","9th Cir","10th Cir","11th Cir","DC","1997","1998","1999","2000","2001","2002"), se = list(my.out1c = NULL, my.out2c = NULL, my.out3c = NULL, my.out4c = NULL, my.out5c = NULL, my.out6c = NULL, my.out7c = bcse11c, my.out3c = bcse12c), digits = 2)


########################################################
## Table 7: Dividing Up the Sample
########################################################

## First, limiting sample space to only judges 
## with 1-4 kids 
#(there is only 1 judge with no girls conditional on 5 or more kids)

#judge.means <- subset(judge.means, child < 5 & child > 0)
rep.means <- subset(rep.means, child < 5 & child > 0)
dem.means <- subset(dem.means, child < 5 & child >0)
men.means <- subset(men.means, child < 5 & child > 0)
women.means <- subset(women.means, child < 5 & child > 0)

## and then by case
rep.cases <- subset(women.cases, child < 5 & child > 0 & republican == 1)
dem.cases <- subset(women.cases, child < 5 & child > 0 & republican == 0)
men.cases <- subset(women.cases, woman == 0)
womyn.cases <- subset(women.cases, woman == 1)


################ Party

my.outPARTY <- lm(lib_vote_share ~ I(girls > 0)*republican + as.factor(child), 
	data = judge.means, weights = judge.means$no_cases)
summary(my.outPARTY)

## for republicans

my.outREP <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = rep.means, weights = rep.means$no_cases)
summary(my.outREP)


## for democrats

my.outDEM <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = dem.means, weights = dem.means$no_cases)
summary(my.outDEM)

################ Gender

## for men
	
my.outGENDER <- lm(lib_vote_share ~ I(girls > 0)*woman + as.factor(child), 
	data = judge.means, weights = judge.means$no_cases)
summary(my.outGENDER)

## for men
	
my.outMEN <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = men.means, weights = men.means$no_cases)
summary(my.outMEN)

## for women

my.outWOMEN <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = women.means, weights = women.means$no_cases)
summary(my.outWOMEN)

my.outREPMEN <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = subset(men.means, republican == 1), weights = men.means$no_cases[which(men.means$republican == 1)])
summary(my.outREPMEN)


stargazer(my.outREP, my.outDEM, my.outMEN, my.outWOMEN, my.outREPMEN,
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Share of Votes in Feminist Direction", digits = 2, covariate.labels = c("At Least 1 Girl", "2 Children", "3 Children", "4 Children","Constant"), title = "Weighted least squares results. Outcome is judges' proportion of feminist votes on gender-related cases. All models include fixed effects for total number of children and use weights based on the number of cases heard by each judge.", label = "t:results_party")


########################################################
## Table 8: Judges with 0 or 1 child
########################################################

## Including people who have zero children as a control 
## suggested by AJPS editor 
## (Note: Caution against making inferences on effect of
## daughters among people with 0 children)

my.out12 <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
	data = subset(judge.means, child < 2), 
		weights = judge.means$no_cases[which(judge.means$child < 2)])
summary(my.out12)

my.out13 <- lm(lib_vote_share ~ I(girls > 0), 
	data = subset(judge.means, child == 1), 
		weights = judge.means$no_cases[which(judge.means$child == 1)])
summary(my.out13)

## and then controlling for rep or dem:

my.out14 <- lm(lib_vote_share ~ I(girls > 0) + I(republican==1), 
	data = subset(judge.means, child == 1), 
		weights = judge.means$no_cases[which(judge.means$child == 1)])
summary(my.out14)


stargazer(my.out12, my.out13, my.out14, 
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Liberal Judge-Vote")



########################################################
## Table 9: Distribution of gender-related cases
########################################################


## just republicans:
rep.means <- subset(judge.means, republican == 1)

## just democrats
dem.means <- subset(judge.means, republican == 0)



aa <- table(dem.means$child, dem.means$girl)
bb <- table(rep.means$child, rep.means$girl)

aa.1 <- prop.table(table(dem.means$child, dem.means$girl),1)
bb.1 <- prop.table(table(rep.means$child, rep.means$girl),1)

colnames(aa.1) <- c("0 Girls", "1 Girl", "2 Girls", "3 Girls", "4 Girls", "5 Girls")

colnames(bb.1) <- c("0 Girls", "1 Girl", "2 Girls", "3 Girls", "4 Girls")

xtable(cbind(aa.1, bb.1))

########################################################
## Supplemental Information Table 1
########################################################

my.out1s <- zelig(progressive.vote ~ I(girls > 0) + as.factor(child), 
		model = "logit", data = subset(women.cases, child < 2))
summary(my.out1s)


my.out2s <- zelig(progressive.vote ~ I(girls > 0), 
		model = "logit", data = subset(women.cases, child == 1))
summary(my.out2s)

## and again controlling for dem or rep

my.out3s <- zelig(progressive.vote ~ I(girls > 0)  + republican, 
		model = "logit", data = subset(women.cases, child == 1))
summary(my.out3s)

stargazer(my.out1s, my.out2s, my.out3s, 
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Liberal Judge-Vote", title = "Logit regression results, gender cases only. Outcome is whether judge in a case votes in a feminist direction. Column 1 includes judges with 0 or 1 child. Columnes 2--3 includes judges with 1 child only.")

########################################################
## Supplemental Information Table 2
########################################################

## subsetting to rape cases, as coded by kuersten haire (includes sexual assault, per their codebook)
rape <- subset(cases, type2 ==102 | type2 == 122 | type2 == 142) 
crime <- subset(cases, type == 1) 
allnongender <- subset(cases, type2 != 231 & type2 != 231 & type2 != 232 & type2 != 233 
				  & type2 != 239 & type2 != 501 & type2 != 503)


## Simple model for rape cases

my.out1s <- zelig(I(vote > 1) ~ I(girls > 0) + as.factor(child), 
		model = "logit", data = subset(rape, child < 5 & child > 0))
summary(my.out1s)

## Simple model plus a few extras for criminal cases

my.out2s <- zelig(I(vote > 1) ~ I(girls > 0) + as.factor(child), 
		model = "logit", data = subset(crime, child < 5 & child > 0))
summary(my.out2s)

my.out3s <- zelig(I(vote > 1) ~ I(girls > 0) + as.factor(child) + as.factor(circuit), 
		model = "logit", data = subset(crime, child < 5 & child > 0))
summary(my.out3s)

my.out4s <- zelig(I(vote > 1) ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year), 
		model = "logit", data = subset(crime, child < 5 & child > 0))
summary(my.out4s)


my.out5s <- zelig(progressive.vote ~ I(girls>0) + as.factor(child) + party 
                   + age + I(religion == 4) + gender + I(race == 2) + I(race == 3)+ as.factor(circuit) + as.factor(year), 
                   data = subset(crime, child < 5 & child > 0), model = "logit")
my.out6s <- zelig(as.factor(vote) ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year), 
                   model = "ologit", data = subset(crime, child < 5 & child > 0))


# To bootstrap the standard errors
mod12 <- as.formula("~ progressive.vote +vote+ girls +  child+ party +race + age + religion + gender + circuit + year + case")
dat12 <- model.frame(mod12, data = subset(crime, child < 5 & child > 0))
dat12$casename <- as.character(dat12$case)

## create list of data.frames for each case for easy reference
casenames <- sort(unique(dat12$casename))
caselist <- lapply(casenames, function(x) dat12[which(dat12$casename == x),])
names(caselist) <- casenames


## Running the bootstraps
## Note that not all of the coefficients will be sampled in any given of the 
## bootstraps - this might result in some warnings form polr
## (the ordered logit), but is not a problem for the results

boots <- 1000
b.star11 <- matrix(NA, ncol =length(my.out5s$result$coef), nrow = boots)
colnames(b.star11) <- names(my.out5s$result$coef)
b.star12 <- matrix(NA, ncol =length(my.out6s$result$coef), nrow = boots)
colnames(b.star12) <- names(my.out6s$result$coef)
set.seed(1234)
for (b in 1:boots) {
  if ((b%%10) == 0) cat("boot: ", b, "\n")
  clust.sample <- sample(casenames, replace = TRUE)
  c.boot <- do.call(rbind,lapply(clust.sample,function(x) caselist[[x]]))
  out11.star <- glm(progressive.vote ~ I(girls>0) + as.factor(child) + party 
                    + age + I(religion == 4) + gender + I(race == 2) + I(race == 3)+ as.factor(circuit) + as.factor(year), data = c.boot, family = binomial("logit"))
  out12.star <- polr(as.factor(vote) ~ I(girls>0) + as.factor(child) + party + age + I(religion == 4) + gender + I(race == 2) + I(race ==3) + as.factor(circuit) + as.factor(year), data = c.boot)	
  b.star11[b,names(out11.star$coef)] <- out11.star$coef
  b.star12[b,names(out12.star$coef)] <- out12.star$coef
}
bcse5.crime <- apply(b.star11, 2, sd, na.rm=TRUE)		
bcse6.crime <- apply(b.star12, 2, sd, na.rm=TRUE)		
#save(list=c("bcse5.crime", "bcse6.crime"), file = "bootstraps-crime.RData")
# End bootstrap code here
load("bootstraps-crime.RData")


stargazer(my.out1s, my.out2s, my.out3s, my.out4s, my.out5s, my.out6s,
	style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Case-Level Judge-Vote in Feminist Direction", covariate.labels = c("At Least 1 Girl","2 Children","3 Children","4 Children","Republican","Age at Investiture","Catholic","Woman","African American","Hispanic",
	"2nd Cir","3rd Cir","4th Cir","5th Cir","6th Cir","7th Cir","8th Cir","9th Cir","10th Cir","11th Cir","DC","1997","1998","1999","2000","2001","2002"), se = list(my.out1 = NULL, my.out2 = NULL, my.out3 = NULL, my.out4 = NULL, my.out5.crime = bcse5.crime, my.ou6.crime = bcse6.crime), digits = 2, title = "Logit and ordered logit regression results for a random subset of rape cases (Model 1) and criminal cases (Models 2--6), as coded by Kuersten and Haire (2012). Outcome is whether a judge in a case votes in a liberal direction (Models 1--5) or in a conservative, mixed, or liberal direction (Model 6). All models include fixed effects for the number of children. Models 3 and 4 include additional case-level controls, including circuit fixed effects (Models 3--6), year fixed effects (Models 4--6), and other predictive covariates (Models 4--6). Under no model specification is the effect on the daughter's coefficient either large or statistically significant, including when subletting by party or by gender (not shown)")

