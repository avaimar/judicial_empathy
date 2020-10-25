library(data.table)
library(ggplot2)
library(dplyr)

data_cases <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_case_1.csv")
data_judges <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_judge.csv")


## subset cases to match method from paper
cases_small = subset(data_cases, area == "employment" | area == "Title IX" | area == "pregnancy" | area == "abortion" | area == "reproductive rights")
cases_small = subset(cases_small, femplaintiff == 1)
cases_small$area <- factor(cases_small$area, levels = c("abortion","employment","pregnancy","reproductive rights","Title IX"))

## taken from authors R code
## get number of cases heard by each judge
no_cases <- matrix(data = NA, nrow = nrow(data_judges), ncol = 1)
for(i in 1:length(no_cases)){
  no_cases[i] <- nrow(cases_small[which(women.cases$name == judge.means$name[i]),])
}
data_judges$no_cases = no_cases
### total number of relevant cases heard by all judges in dataset
sum(data_judges$no_cases)

### subset the data to include only those judges for whom we
### fertility data
judges_small = subset(data_judges, !is.na(girls))
nrow(judges_small)

## number of cases heard by judges who we have fertility data about
cases_fertility = subset(cases_small, name %in% judges_small$name)
nrow(cases_fertility)

colSums(is.na(data_judges[, .(child)]))
colSums(is.na(data_judges[, .(girls)]))
colSums(is.na(data_judges[, .(sons)]))

mean_na = colMeans(is.na(judges_small))
mean_na[mean_na != 0]
## highest proportion of NAs is age (.24), followed by religion, then birth year and race
## NOTE: I think we could just get rid of the age variable since it has more NAs than birth
## year conveys the same information. 

summary(judges_small)
## ranges of attributes look reasonable, doesn't look like there are any
## obvious erroneous entries

p <- ggplot(judges_small, aes(x=as.factor(circuit.1))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p

## very few judges who have had more than 5 children
p2 <- ggplot(judges_small, aes(x=child)) + 
  geom_histogram(fill = 'lightblue', bins = 9)
p2

p3 <- ggplot(judges_small, aes(x=as.factor(woman))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p3

## only one person in the race 4 category, 6 in race 3
## I still think this will not be an issue in analyses.
## I think it just means match might not be as good for these individuals
judges_small %>% 
  group_by(race) %>%
  summarise(no_rows = length(race))
p4 <- ggplot(judges_small, aes(x=as.factor(race))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p4

p5 <- ggplot(judges_small, aes(x=age)) + 
  geom_histogram(fill = 'lightblue', bins=30)
p5

## only one instance of religions 9, 11, 16, 17, 21, 24
judges_small %>% 
  group_by(religion) %>%
  summarise(no_rows = length(religion))
p6 <- ggplot(judges_small, aes(x=as.factor(religion))) + 
  geom_histogram(fill = 'lightblue', stat='count')
p6

## if these categories were consistent, we would expect age + yearb to be around
## the same value for all judges. Perhaps this is because the data was taken from
## different sources and at different times so the age relative to different years
levels(as.factor(judges_small$age + judges_small$yearb))

# drop "yearb" column
judges_small = judges_small[,-9]

## add indicator variables for missing values
judges_small$agemiss = as.numeric(is.na(judges_small$age))
judges_small$religmiss = as.numeric(is.na(judges_small$religion))
judges_small$racemiss = as.numeric(is.na(judges_small$race))

## convert missing values to 0s
judges_small[is.na(judges_small)] <- 0

write.csv(judges_small, 'Judicial_empathy/01_data/02_Cleaned_data/judges_cleaned.csv')
