
# 0. Working set up -----------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(varhandle)
library(glmnet) # lasso

# 1. Load data ---------------------------------
data_cases <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_case_1.csv", check.names = TRUE)
data_judges <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_judge.csv", check.names = TRUE)

## 2. Subset cases to match method from paper -------
cases_small = subset(data_cases, area == "employment" | area == "Title IX" | area == "pregnancy" | area == "abortion" | area == "reproductive rights")
cases_small = subset(cases_small, femplaintiff == 1)
cases_small$area <- factor(cases_small$area, levels = c("abortion","employment","pregnancy","reproductive rights","Title IX"))

# 3. Judge data -------------------------------
## get number of cases for each judge
cases_per_judge <- cases_small[,.(no_cases = .N), by=name]
data_judges <- merge(data_judges, cases_per_judge, by='name')

### total number of relevant cases heard by all judges in dataset
sum(data_judges$no_cases)

## get liberal vote share (outcome)
liberal_votes = cases_small %>% 
  group_by(name) %>% 
  summarise(lib_vote_share = sum(vote == 2 | vote == 3)/n())
## this is the same as 'progressive.vote' in the dataset

### subset the data to include only those judges for whom we
### fertility data
judges_small = subset(data_judges, !is.na(girls))
nrow(judges_small)

## write initial dataset to file for replication purposes
write.csv(judges_small, 'Judicial_empathy/01_data/02_Cleaned_data/judges_replication.csv')

## number of cases heard by judges who we have fertility data about
cases_fertility = subset(cases_small, name %in% judges_small$name)
nrow(cases_fertility)

colSums(is.na(data_judges[, .(child)]))
colSums(is.na(data_judges[, .(girls)]))
colSums(is.na(data_judges[, .(sons)]))

mean_na = colMeans(is.na(judges_small))
mean_na[mean_na != 0]
## highest proportion of NAs is age (.24), followed by religion, then birth year and race

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
judges_small = judges_small[, c("yearb"):=NULL]

## add indicator variables for missing values
judges_small$agemiss = as.numeric(is.na(judges_small$age))
judges_small$religmiss = as.numeric(is.na(judges_small$religion))
judges_small$racemiss = as.numeric(is.na(judges_small$race))

## convert missing values to 0s
judges_small[is.na(judges_small)] <- 0

## convert categorical attributes to binary indicators
race_indicator <- to.dummy(judges_small$race, "race")
judges_small = cbind(judges_small, race_indicator)

judges_small[judges_small$religion %in% c(9, 11, 16, 17, 21, 24)]$religion <- 9
relig_indicator <- to.dummy(judges_small$religion, "religion")
judges_small = cbind(judges_small, relig_indicator)

# For circuit, correct name first so as to not create duplicate column
setnames(judges_small, 'circuit.1', 'circuit')
circuit_indicator <- to.dummy(judges_small$circuit, "circuit")
judges_small = cbind(judges_small, circuit_indicator)

## drop non-dummy columns
#judges_small = judges_small[, c("race"):=NULL]
#judges_small = judges_small[, c("religion"):=NULL]
#judges_small = judges_small[, c("circuit.1"):=NULL]
judges_small <- judges_small[, V1 := NULL]

# Define treatment
# 1. Remove judges with no children
judges_small <- judges_small[child > 0]

# 2. Treatment: Judge has at least 1 girl
judges_small <- judges_small[, z := as.numeric(girls > 0)]

# 3. Move treatment to front
setcolorder(judges_small, c('name', 'z'))

# Write cleaned dataset
write.csv(judges_small, 'Judicial_empathy/01_data/02_Cleaned_data/judges_cleaned.csv',
          row.names = FALSE)

# 4. Case data -----------------------------------

# Save a replication dataset
write.csv(cases_small, 'Judicial_empathy/01_data/02_Cleaned_data/cases_replication.csv',
          row.names = FALSE)

# Get unique columns
cases_small <- cases_small[, .SD, .SDcols = unique(colnames(cases_small))]

# Drop female plaintiff col as we filtered for femaleplaint == 1
cases_small <- cases_small[, femplaintiff := NULL]

# Birth seems to be the same column as yearb except for missing values. We will
# drop birth and keep yearb as this was the variable used in the judge data
sum(cases_small$birth == cases_small$yearb, na.rm = TRUE)
cases_small <- cases_small[, birth := NULL]

# Notice that these 8 variables are pair mirrors of each other (with +-1 differences).
# We keep those in the Codebook.
#View(cases_small[, .( sdem, srep, hdem, hrep)])
cases_small <- cases_small[, c('dsen', 'rsen', 'dhouse', 'rhouse') := NULL]

# Drop columns with dates in "../../.." format
date_columns <- colnames(cases_small)[grepl('/', cases_small)]
cases_small <- cases_small[, c(date_columns) := NULL]

# * 4.1 Add treatment column ------------------------
cases_small <- merge(cases_small, 
                     judges_small[, .(songerID, z)], 
                     by = 'songerID', all.x = TRUE)

# Check units with missing treatments
sum(is.na(cases_small$z))
missing_judges <- unique(cases_small[is.na(z)]$name)
length(missing_judges)

# Why are these missing treatment?
# Only 45 judges were in the judge data
sum(missing_judges %in% data_judges$name)
missing_judges_in_data <- missing_judges[missing_judges %in% data_judges$name]

# These 45 judges either have no children or no info on girls/boys
table(data_judges[name %in% missing_judges_in_data,]$child, 
      data_judges[name %in% missing_judges_in_data,]$girls)

# Therefore, we drop these units with missing treatments
cases_small <- cases_small[!is.na(z)]

# * 4.2 Check missings ---------------------------
missing_cols <- data.table(
  column = names(colMeans(is.na(cases_small))),
  missing_perc = colMeans(is.na(cases_small)))
hist(missing_cols$missing_perc)

# Drop columns with over 85% missing values
sum(missing_cols$missing_perc >= 0.85)
cases_small <- 
  cases_small[, .SD, .SDcols = missing_cols[missing_perc < 0.85,]$column]

# Notice that by dropping to columns with less than 30% missing values
# we are simply removing 245 - 223 = 22 columns
cases_small <- 
  cases_small[, .SD, .SDcols = missing_cols[missing_perc < 0.3,]$column]

# * 4.3 Classify covariates --------------------------
covariates <- colnames(cases_small)

# Drop treatment and outcome
covariates <- covariates[!(covariates %in% c('vote', 'progressive.vote', 'z'))]

# 1. Covariates at the judge level in judge dataset
cov_1_judges <- c('girls', 'sons', 'woman', 'child', 'race', 'republican', 
                  'circuit', 'circuit.1', 'age', 'religion', 'yearb', 'songerID')

# 2. Covariates identifying the cases
#   * casename, name, name.1, capsnames, pname, cite, docket
cov_2_case_ID <- c('casename', 'name', 'name.1', 'capsnames', 'pname', 'cite', 'docket')

# 3. Additional judge-level covariates
cov_3_add_judges_appoint_info <- 
  c('seatno', # ID or seat number
    'amon', # month of confirmation
    'ayear', # year of confirmation
    'pres', # appointing president
    'appres', # party of appointing president
    'city', # city of residence of judge
    'state', # state of residence of judge
    'aba', # American bar association rating
    'abamin', # ABA rating, minority
    'recess', # recess appointment
    'party', # party affiliation of the judge
    'crossa', # judge/apointing president same/different
    'ageon', # age at time of commission
    'congresi', # congress in which appointment occurred
    'hdem', # # of house democrats in year of appointment
    'hrep', # # of house republicans in year of appointment
    'hother', # # of house members of other parties in year of appt.
    'sdem', # # of senate democrats in year of appt.
    'srep', # # of senate republicans in year of appt.
    'sother', # # of senate members of other parites in year of appt.
    'unityi', # whether congress/president unified or divided
    'pseatno' # ID of judge's predecessor
    )

cov_3_add_judges_leaving_info <-
  c('monl', # month of deparment
    'yearl', # year of departure
    'left', # means of exiting
    'pleft', # president in office when judge left bench
    'sseatno' # ID of judge's successor
    )

cov_3_add_judges_background_info <-
  c('yeard', # year of death
    'csb', # city and state of birth
    'ba', # BA degree institution
    'bast', # whether BA received in same state as appt.
    'bapp', # whether ba from public of private inst.
    'ls', # law degree institution
    'lsst', # whether law degree in same state as appt.
    'jdpp', # whether law degree from public or private
    'graddeg1', # first graduate degree (other than JD)
    'graddeg2', # second graduate degree (other than JD)
    'graddeg3' # third graduate degree (other than JD)
    )

cov_3_add_judges_prior_experience <- 
  c('pssc', # sate high court judge
    'pslc', # state lower court judge
    'plocct', # local/municipal court judge
    'psjdget', # years of service as state/local judge
    'pausa', # assistant US attorney
    'pusa', # US attorney
    'psgo', # Solicitor General's office
    'psg', # the solicitor general
    'pago', # justice department
    'pag', # the attorney general
    'pcc', # congressional counsel
    'psp', # special prosecutor
    'pmag', # US magistrate
    'pbank', # bankruptcy judge
    'pterr', # territorial judge
    'pcab', # cabinet secretary
    'pcabdept', # if cabinet secretary, which dept.?
    'pscab', # subcabinet secretary
    'pscabdept', # if subcabinet secretary, which dept.?
    'paag', # subcabinet secretary, dept of just
    'pindreg1', # other federal experience
    'preg1', # If so, what?
    'preg2', # second other federal experience
    'preg3', # third other federal experience
    'phouse', # US house of representatives
    'psenate', # US senate
    'pgov', # governor
    'statecab', # state cabinet or other state office?
    'pssenate', # state senate
    'pshouse', # state house
    'pmayor', # mayor
    'pccoun', # city council
    'pccom', # county commission
    'pada', # deputy or assistant district/county/city
    'pda', # district/county/city attorney
    'plother', # other local experience
    'plotherl', # if so, what position?
    'plawprof', # full-time law professor
    'pprivate', # private practice
    'prevpos' # posiiton held at time of appoint.
    )

# 4. Case-info covariates
cov_4_case_info <- 
  c('year', # date in which decision was announced
    'source1', # forum that held this case immediately before case came to court of appeals
    'source2', # could not understand difference with above variable
    'area'
    )

# Remaining covariates to classify
covariates[!(covariates %in%c(cov_1_judges, cov_2_case_ID, cov_3_add_judges_appoint_info,
                              cov_3_add_judges_background_info, cov_3_add_judges_leaving_info,
                              cov_3_add_judges_prior_experience, cov_4_case_info))]


# Keep cov_1_judges, cov_4_case info, treatment and outcome, and V1 as the
# unique case ID
cases_small <- cases_small[, .SD, .SDcols = 
                             c('V1', cov_1_judges, cov_4_case_info, 'z', 'progressive.vote')]
setnames(cases_small, 'V1', 'case_ID')

# * 4.4. Final changes ---------------------
# Drop source1 and source2 as these have a single value, and yearb
cases_small <- cases_small[, c('source1', 'source2', 'yearb') := NULL]

# Note that for 9% of cases circuit and circuit.1 differ. We stay with circuit.1
# as this is the column used for the judge data and as circuit has some strings
# that aren't mapped to circuit numbers.
cases_small <- cases_small[, circuit:= NULL]
setnames(cases_small, 'circuit.1', 'circuit')

# We have one case with missing value for the outcome. 
cases_small <- cases_small[case_ID != 1693]

# Create missing dummies
## Add indicator variables for missing values
cases_small$agemiss <- as.numeric(is.na(cases_small$age))
cases_small$religmiss <- as.numeric(is.na(cases_small$religion))
cases_small$racemiss <- as.numeric(is.na(cases_small$race))
cases_small$circuitmiss <- as.numeric(is.na(cases_small$circuit))

## convert missing values to 0s
cases_small[is.na(cases_small)] <- 0

## convert categorical attributes to binary indicators
race_indicator <- to.dummy(cases_small$race, "race")
cases_small <- cbind(cases_small, race_indicator)

cases_small[cases_small$religion %in% c(9, 11, 16, 17, 21, 24)]$religion <- 9
relig_indicator <- to.dummy(cases_small$religion, "religion")
cases_small <- cbind(cases_small, relig_indicator)

# For circuit, correct name first so as to not create duplicate column
circuit_indicator <- to.dummy(cases_small$circuit, "circuit")
cases_small <- cbind(cases_small, circuit_indicator)

# * 4.5 Write cleaned dataset --------------------
write.csv(cases_small, 'Judicial_empathy/01_data/02_Cleaned_data/cases_cleaned.csv',
          row.names = FALSE)
