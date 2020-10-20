# INFO -------------------------------------------
# Script: 01_Exploratory_Analysis.R
# Inputs:
# Outputs:
# ------------------------------------------------


# 0. Working set up ------------------------------
# Libraries
library(data.table)

# 1. Load data ------------------------------------
data_cases <- fread("Judicial_empathy/01_Raw_data/glynn_sen_daughters_by_case_1.csv")
data_judges <- fread("Judicial_empathy/01_Raw_data/glynn_sen_daughters_by_judge.csv")
