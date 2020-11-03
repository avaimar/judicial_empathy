# INFO -------------------------------------------
# Script: 06_Analysis.R
# Inputs:
#   * Modified judge data (including matches)
#     File: Judicial_empathy/03_Output/03_Matching/
#   * Cleaned case data
#     File: Judicial_empathy/01_data/02_Cleaned_data/cases_cleaned.csv
# Outputs:
#   * Plots and latex tables
#     Location: Judicial_empathy/03_Output/04_Analysis

# 0. Working set up ------------------------------
# Libraries
library(data.table) # Table manipulation
library(DOS2) 
library(optmatch)
library(sensitivitymult)
library(ggplot2) # Plots

# Output directory
output <- 'Judicial_empathy/03_Output/04_Analysis/'

# Helper functions
source('Judicial_empathy/02_Scripts/00_utility.R')

# Parameters
selected_match <- 'Judicial_empathy/03_Output/03_Matching/[INSERT MATCH HERE].csv'

# 1. Load data ------------------------------------
# Matched judge data
data_judges <- fread(selected_match)

# Cleaned case data
data_cases <- fread('Judicial_Empathy/01_Data/02_Cleaned_data/cases_cleaned.csv')

# 2. Judge data -----------------------------------

# * 2.1 FRT ---------------------------------------

# * 2.2 Sensitivity analysis ----------------------

# * 2.3 Sensibility analysis ----------------------

# * 2.4 Heterogeneous effects ---------------------

# 3. Case data ------------------------------------
# * 3.1 Obtain residuals --------------------------

# * 3.2 FRT ---------------------------------------

# * 3.3 Sensitivity analysis ----------------------

# * 3.4 Sensibility analysis ----------------------
