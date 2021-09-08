### Homework 2: Phase 2

# set the working directory
setwd("C:/Users/alexr/Documents/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework2_LR")

# import libraries
library(ggplot2)
library(tidyverse)
library(gmodels)

# read in the data sets
ins_t_bin = read_csv("insurance_t_bin.csv")
ins_v_bin = read_csv("insurance_v_bin.csv")

# subset the data (MMCRED - HMOWN)
my_vars <- ins_t_bin %>%
  select(c("MMCRED", "MTG", "MTGBAL_Bin", "CC", "CCBAL_Bin",
           "CCPURC", "SDB", "INCOME_Bin", "HMOWN", "INS"))


## for any variables with missing values, change the data to include a missing category instead of a missing variable
## this effectively casts these variables from double to character

# impute missing values for CC variable
my_vars_imp <- my_vars %>%
  mutate("CC" = ifelse(is.na(CC), "M", CC))

# impute missing values for CCPURC variable
my_vars_imp <- my_vars_imp %>%
  mutate("CCPURC" = ifelse(is.na(CCPURC), "M", CCPURC))

# impute missing values for HMOWN variable
my_vars_imp <- my_vars_imp %>%
  mutate("HMOWN" = ifelse(is.na(HMOWN), "M", HMOWN))


## check each variable for separation concerns

# make cross tabulation tables between response and each predictor
for (i in 1 : (length(my_vars_imp) - 1)) {
  # extract column from the data frame
  col <- my_vars_imp[,colnames(my_vars_imp)[i]]
  # extract the values from the column
  var <- col[[1]]
  # create a cross tabulation between INS values and column values
  print(table(my_vars_imp$INS, var))
}

# Quasi-complete separation occurs on the MMCRED variable

# collapse levels 3 and 5 into one level to address quasi-separation
my_vars_imp <- my_vars_imp %>%
  mutate(MMCRED = ifelse(MMCRED > 2, "3+", as.character(MMCRED)))
