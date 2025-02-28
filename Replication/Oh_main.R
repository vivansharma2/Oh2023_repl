# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# PROJECT:		 Replication of "Does Identity Affect Labor Supply?" (Oh, 2023)
# REPLICATORS: Vivan Sharma, Rodrigo Pereira
# TASK:				 Replicate project in R
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load packages
library(haven)
library(dplyr)
library(stringr)
library(labelled)
library(tidyr)
library(ggplot2)
library(patchwork)
library(fixest)
library(modelsummary)
library(RPregression)

# Clear all
rm(list = ls())

# Set relative filepaths
rawdata <- "/Users/vivansharma/Desktop/Oh(2023)/Data/raw/"
cleandata <- "/Users/vivansharma/Desktop/Oh(2023)/Data/clean/"
output <- "/Users/vivansharma/Desktop/Oh(2023)/Output/"
code <- "/Users/vivansharma/Desktop/Oh(2023)/Replication/"

setwd("/Users/vivansharma/Desktop/Oh(2023)/Replication/") # Set working directory

source("0_cleaning.R")
source("1_analysis.R")
source("2_addclean.R")
source("3_addanalysis.R")
