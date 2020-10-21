# INFO -------------------------------------------
# Script: 01_Exploratory_Analysis.R
# Inputs:
#   * Case data: Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_case_1.csv
#   * Judge data: Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_judge.csv
# Outputs:
#   * Exploratory plots at 03_Outputs/01_EDA/

# 0. Working set up ------------------------------
# Libraries
library(data.table)
library(ggplot2)

# Output directory
output <- 'Judicial_empathy/03_Output/01_EDA/'

# 1. Load data ------------------------------------
data_cases <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_case_1.csv")
data_judges <- fread("Judicial_empathy/01_Data/01_Raw_data/glynn_sen_daughters_by_judge.csv")

# Drop outcomes so as to not contaminate analysis
data_judges<- data_judges[, progressive.vote := NULL]
data_cases <- data_cases[, vote:= NULL]

# CODEBOOK: Data Judges
# * yearb: Year of birth
# * race: race of judge (African American, Caucasian, Asian American, 
# Hispanic, Native American)
# * religion: relligion

# CODEBOOK: Data Cases
# year: date on which the decision was announced
# circuit: circuit of the court that decided the case
# state: state in which the case was first heard
# yeard year of death
# pssc: state high court judge
# plocct: local/municipal court judge
# psjdget: years of service as a state/local judge
# pausa: assistant US attorney
# graddeg1: first graduate degree
# graddeg2: second graduate degree
# jdpp: whether law degree from public or private
# ls: law degree institution
# bast: whether BA received in same state as appointment
# ba: B.A. degree institution
# pusa: US Attorney
# psgo: solicitor general's office
# psg: the Solicitor General
# pago: Justice department
# ... many more from the Multi-User Database.. codebook

# Shared variables
# name: judge name
# child
# girls, sons
# woman
# yearb was recoded as birth
# republican
# songerID

# 2. Explore data ----------------------------------
# * 2.1 Judge data ------------------------------------
# Summarize
str(data_judges)
summary(data_judges)

# Verify unit of analysis
length(unique(data_judges[, name])) == dim(data_judges)[1]

# Identify missing values
colSums(is.na(data_judges))
colMeans(is.na(data_judges))

# ** Children ----------------------------------------------
# Verify child variable validity
# Note: There are 20 judges for which we do not know boy/girl
# child decomposition, and 9 judges whose number of children is unknown.
colSums(data_judges[, .(child)] == data_judges[, .(girls)] + data_judges[, .(sons)], 
        na.rm = TRUE)
colSums(data_judges[, .(child)] != data_judges[, .(girls)] + data_judges[, .(sons)], 
        na.rm = TRUE)
colSums(is.na(data_judges[, .(child)]))
colSums(is.na(data_judges[, .(girls)]))
colSums(is.na(data_judges[, .(sons)]))

# Histogram of number of children
g <- ggplot(data_judges, aes(x = child)) +
  geom_histogram(bins = 30, fill = '#0072B2') +
  theme_classic() + 
  labs(x="Number of children", y= "Count") +
  ggtitle("Histogram: Number of children per judge")
ggsave(paste0(output,'hist_children.png'),
       units='in', width=6, height=5, scale=1.1)

# Child gender
colMeans(data_judges[, .(girls)], na.rm=TRUE)
colMeans(data_judges[, .(sons)], na.rm=TRUE)
# Does this say anything about fertility stopping?

# Histogram of number of children (by gender)
g <- ggplot(data_judges) +
  geom_histogram(aes(x=sons, y= ..density..), fill='#00AFBB')+
  geom_histogram(aes(x=girls, y = -..density..), fill = '#E7B800')+
  annotate('text', x = 5, y = 1.8, label = 'Girls', color = '#E7B800') +
  annotate('text', x = 5, y = 2, label = 'Sons', color = '#00AFBB') +
  theme_classic() + 
  labs(x="Number of children", y= "Count") +
  ggtitle("Number of children per judge (by child gender)")
ggsave(paste0(output,'hist_children_gender.png'),
       units='in', width=6, height=5, scale=1.1)

# Number of girls
data_judges[, .(size = .N, percentage = .N * 100/ dim(data_judges)[1]), 
            by = factor(girls)]

# ** Gender ------------------------------------------
# Number of children per judge's sex
g <- ggplot() +
  geom_histogram(data = data_judges[woman==0], 
                 aes(x=child, y = ..density..), 
                 position = 'identity', alpha = 0.7,
                 fill= '#999999') +
  geom_histogram(data = data_judges[woman==1], 
                 aes(x=child, y = -..density..), 
                 position = 'identity', alpha = 0.7,
                 fill= '#009E73') +
  annotate('text', x = 12, y = 0.46, label = 'Male', color = '#999999') +
  annotate('text', x = 12, y = 0.40, label = 'Female', color = '#009E73') +
  theme_classic() +
  labs(x='Number of children', y = 'Count') +
  ggtitle("Number of children by judge's sex")
ggsave(paste0(output, 'hist_children_judge_gender.png'),
       units='in', width=6, height=5, scale=1.1)

# ** Age ---------------------------------------------------
g <- ggplot(data = data_judges, aes(x=age)) +
  geom_histogram() + 
  theme_classic() +
  labs(x= 'Age', y = 'Judge Count') +
  ggtitle("Histogram: Judge age")
ggsave(paste0(output, 'hist_judge_age.png'),
       units='in', width=6, height=5, scale=1.1)

# ** Judge subpopulations ---------------------------
# Gender
data_judges[, .(subpopulation_size = .N,
                girl_percentage = sum(girls, na.rm=TRUE) / sum(child, na.rm=TRUE)), 
            by = .(woman)]

# Race
data_judges[, .(subpopulation_size = .N,
                girl_percentage = sum(girls, na.rm=TRUE) / sum(child, na.rm=TRUE)),
            by = .(race)]

# Religion
data_judges[, .(subpopulation_size = .N,
                girl_percentage = sum(girls, na.rm=TRUE) / sum(child, na.rm=TRUE)), 
            by = .(religion)]

# Partisanship
data_judges[, .(subpopulation_size = .N,
                girl_percentage = sum(girls, na.rm=TRUE) / sum(child, na.rm=TRUE)), 
            by = .(republican)]

# * 2.2 Case data ------------------------------------
# Identify missing values
colSums(is.na(data_cases))
colMeans(is.na(data_cases))

# Number of judges
length(unique(data_cases[, name]))

# Case area
data_cases[, .(number = .N,
               percentage_total = .N * 100/ dim(data_cases)[1]), 
           by = factor(area)]

# Case judge
data_cases[, .(number = .N,
               percentage_total = .N * 100/ dim(data_cases)[1]), 
           by = factor(name)]

# Case plaintiff?
# New Seat?
data_cases[, .(number = .N,
               percentage_total = .N * 100/ dim(data_cases)[1]), 
           by = factor(pname)]

# Find duplicate columns
cols_cases <- as.data.table(colnames(data_cases))
duplicate_cols <- cols_cases[, .N, by = V1]
duplicate_cols <- duplicate_cols[N > 1]

