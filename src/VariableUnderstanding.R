### Variable Understanding

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework1_LR")

# import libraries
library(ggplot2)
library(tidyverse)
library(vcdExtra)
library(DescTools)
library(mgcv)

# display full decimal value instead of using scientific notation
options(scipen = 999)

# read in the data sets
ins_t = read.csv("insurance_t.csv")
ins_v = read.csv("insurance_v.csv")



###################################################################################################

## Part1

# classify the variables...

# binary: DDA, DIRDEP, NSF, SAV, ATM, CD, IRA, LOC, INV, ILS, MM, MTG, CC,
#         SDB, HMOWN, MOVED, INAREA, INS

# ordinal: CASHBK, MMCRED, CCPURC

# nominal: BRANCH, RES

# continuous: ACCTAGE, DDABAL, DEP, DEPAMT, CHECKS, NSFAMT, PHONE, 
#             TELLER, SAVBAL, ATMAMT, POS, POSAMT, CDBAL, IRABAL, LOCBAL,
#             INCOME, LORES, HMVAL, AGE, CRSCORE, 
#             INVBAL, ILSBAL, MMBAL, MTGBAL, CCBAL


# explore the association between response and binary predictors

# define a list of binary variable names
bins <- c("INS", "DDA", "DIRDEP", "NSF", "SAV", "ATM", "CD", "IRA", "LOC", 
          "INV", "ILS", "MM", "MTG", "CC", "SDB", "HMOWN", "MOVED", "INAREA")

# create a new data frame of binary variables
bin_df <- ins_t[,bins]

# create an empty data frame to fill
df <- data.frame(variable = character(), pval = double(),
                 class = character(), test = character())

# for each binary predictor variable
for (i in 1:length(colnames(bin_df)) - 1) {
  # run a measure of association between predictor and response
  df[i, "pval"] <- CMHtest(table(bin_df[,1], bin_df[,(i+1)]))$table[1,3]
  # fill data frame with other attributes
  df[i,"variable"] <- colnames(bin_df)[i+1]
  df[i, "class"] <- "Binary"
  df[i, "test"] <- "MH"
  
  #print(CMHtest(table(bin_df[,1], bin_df[,(i+1)]))$table[1,])
}


# explore the association between response and ordinal predictors

# define a list of ordinal variable names
ords <- c("INS", "CASHBK", "MMCRED", "CCPURC")

# create a new data frame of ordinal variables
ord_df <- ins_t[,ords]

# create an empty data frame to fill
df2 <- data.frame(variable = character(), pval = double(),
                 class = character(), test = character())

# for each ordinal predictor variable
for (i in 1:length(colnames(ord_df)) - 1) {
  # run a measure of association between predictor and response
  df2[i, "pval"] <- CMHtest(table(ord_df[,1], ord_df[,(i+1)]))$table[1,3]
  # fill data frame with other attributes
  df2[i,"variable"] <- colnames(ord_df)[i+1]
  df2[i, "class"] <- "Ordinal"
  df2[i, "test"] <- "MH"
  
  #print(CMHtest(table(ord_df[,1], ord_df[,(i+1)]))$table[1,])
}


# explore the association between response and nominal predictors

# define a list of nominal variable names
noms <- c("INS", "BRANCH", "RES")

# create a new data frame of nominal variables
nom_df <- ins_t[,noms]

# create an empty data frame to fill
df3 <- data.frame(variable = character(), pval = double(),
                  class = character(), test = character())

# for each nominal predictor variable
for (i in 1:length(colnames(nom_df)) - 1) {
  # run a measure of association between predictor and response
  df3[i, "pval"] <- chisq.test(table(nom_df[,1], nom_df[,(i+1)]))$p.value
  # fill data frame with other attributes
  df3[i,"variable"] <- colnames(nom_df)[i+1]
  df3[i, "class"] <- "Nominal"
  df3[i, "test"] <- "Chi Squared"
  
  #print(chisq.test(table(nom_df[,1], nom_df[,(i+1)])))
}


# explore association between response and continuous predictors

# use likelihood ratio test or logistic regression here
# (can only use likelihood ratio test when comparing nested models)

# define a list of continuous variable names (exclude SAVBAL)
conts <- c("INS", "ACCTAGE", "DDABAL", "DEP", "DEPAMT", "CHECKS", "NSFAMT",
           "PHONE", "TELLER", "ATMAMT", "POS", "POSAMT", "CDBAL", "IRABAL",
           "LOCBAL", "INCOME", "LORES", "HMVAL", "AGE", "CRSCORE", "INVBAL",
           "ILSBAL", "MMBAL", "MTGBAL", "CCBAL")

# create a new data frame of continuous variables
conts_df <- ins_t[,conts]

# create an empty data frame to fill
df4 <- data.frame(variable = character(), pval = double(),
                  class = character(), test = character())

# for each continuous predictor variable
for (i in 2:length(colnames(conts_df)) - 1) {
  # fit a logistic regression between predictor and response
  fit <- glm(conts_df[,1] ~ conts_df[,(i+1)], data = conts_df,
             family = binomial(link = "logit"))
  stats <- summary(fit)
  
  # populate data frame
  df4[i, "pval"] <- stats$coefficients[2,4]
  df4[i, "variable"] <- colnames(conts_df)[i+1]
  df4[i, "class"] <- "Continuous"
  df4[i, "test"] <- "Logistic Regression"
  
  #print(summary(fit))
}

# Causing fitted probabilities of 0 or 1: SAVBAL, confirmed that this is okay,
# probability is within machine precision of 1.

#fit_gam <- gam(INS ~ as.factor(SAVBAL), data = ins_t,
#               family = binomial(link = "logit"), method = "REML")


# join the dataframes
tbl <- rbind(df, df2, df3, df4)

# sort by p values
tbl_sorted <- tbl %>%
  arrange(pval)

# filter out rows with pval < 0.002
tbl_sig <- tbl_sorted %>%
  filter(pval < 0.002)

# write table to a csv file
write.csv(tbl_sig, "C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework1_LR/predictors.csv", row.names = F)



###################################################################################################

## Part 2

# create an empty data frame to fill
df5 <- data.frame(variable = character(), odds_ratio = double())

# calculate odds ratio for each each binary predictor in relation to response
for (i in 1:length(colnames(bin_df)) - 1) {
  # populate data frame
  df5[i, "odds_ratio"] <- OddsRatio(table(bin_df[,1], bin_df[,(i+1)]))
  df5[i, "variable"] <- colnames(bin_df)[i+1]
  
  #print(OddsRatio(table(bin_df[,1], bin_df[,(i+1)])))
}

# sort by descending magnitude
tbl_odds <- df5 %>%
  arrange(desc(odds_ratio))

# write table to a csv file
write.csv(tbl_odds, "C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework1_LR/odds.csv", row.names = F)

# customers with an investment account are 3.47 times more likely to
# purchase an insurance product then customers without an investment
# account

# observing the table of odds ratios, it appears that customers who 
# actively participate in some form of saving or investment have 
# higher odds of purchasing an annuity than customers who do not.



###################################################################################################

# Part 3

# provide a summary of results around the linearity assumption of continuous variables

# for each continuous predictor variable
for (i in 2:length(colnames(conts_df)) - 1) {
  # fit a general additive model using a spline function
  fit_gam <- gam(conts_df[,1] ~ s(conts_df[,(i+1)]), data = conts_df,
                 family = binomial(link = 'logit'), method = 'REML')
  
  # print name of predictor
  print(colnames(conts_df)[i+1])
  # print a summary of the model
  print(summary(fit_gam))
  # plot the spline function
  plot(fit_gam)
  title(main = colnames(conts_df)[i+1])
}


# check assumptions by using a chi-square test to compare linear version of continuous
# variables to spline version

# create empty data frames to fill
df6 <- data.frame(variable = character(), pval = double())
df7 <- data.frame(variable = character(), pval = double())

# for each continuous predictor variable
for (i in 2:length(colnames(conts_df)) - 1) {
  
  # fit a linear version of the predictor
  fit <- glm(conts_df[,1] ~ conts_df[,(i+1)], data = conts_df,
             family = binomial(link = "logit"))
  
  # fit a spline version of the predictor
  fit_gam <- gam(conts_df[,1] ~ s(conts_df[,(i+1)]), data = conts_df,
                 family = binomial(link = 'logit'), method = 'REML')
  
  # use a chi-square test to compare them (H_0: linear model in sufficient (linearity met))
  test <- anova(fit, fit_gam, test = "Chisq")
  
  # if pval > 0.002, assumption is met and add to df6
  if (test[2,5] > 0.002) {
    
    # populate the data frame
    df6[i, "pval"] <- test[2,5]
    df6[i, "variable"] <- colnames(conts_df)[i+1]
    
  # else, assumption is not met and add to df7
  } else {
    
    # populate the data frame
    df7[i, "pval"] <- test[2,5]
    df7[i, "variable"] <- colnames(conts_df)[i+1]
  }
}

# remove NAs from data frames and sort by descending pval
tbl_lin <- df6 %>%
  filter(!is.na(pval)) %>%
  arrange(desc(pval))

tbl_nonlin <- df7 %>%
  filter(!is.na(pval)) %>%
  arrange(desc(pval))

# write tables to a csv file
write.csv(tbl_lin, "C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework1_LR/linear.csv", row.names = F)

write.csv(tbl_nonlin, "C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework1_LR/nonlinear.csv", row.names = F)

# TODO: use 0.002 for an alpha level? (check with teammates)
# TODO: HMVAL seems to be misclassified



###################################################################################################

# Part 4

# create a visualization of the variables that have the highest percentage of missing values

# create a data frame of the sum of NA values for each column in ins_t 
tbl_na <- ins_t %>%
  summarize_all(funs(sum(is.na(.))))

# list variables we want (nonzero)
vars <- c("ACCTAGE", "PHONE", "POS", "POSAMT", "INV", "INVBAL", "CC", "CCBAL",
          "CCPURC", "INCOME", "HMOWN", "LORES", "HMVAL", "AGE", "CRSCORE")

# select these variables and redefine data frame
tbl_na <- tbl_na %>%
  select(vars)

# use dplyr to plot the output
tbl_na %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("id") %>%
  ggplot(aes(x = reorder(id, V1), y = V1)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Variable Name") +
  ylab("Number of Missing Values (NA)")

# redundant variables: (DDA, DDABAL), (SAV, SAVBAL), (ATM, ATMAMT), (CD, CDBAL),
#                      (IRA, IRABAL), (LOC, LOCBAL), (INV, INVBAL), (ILS, ILSBAL),



#                      (MM, MMBAL), (MTG, MTGBAL), (CC, CCBAL), (CCBAL, CCPURC),
#                      (INCOME, AGE), (HMOWN, HMVAL), (LORES, AGE), (INCOME, HMVAL),
#                      (HMOWN, AGE), (HMVAL, AGE), (MTG, HMOWN), (LOC, CC)?

# create pairs plots of continuous variables
pairs(conts_df[1:5])
pairs(conts_df[6:10])
pairs(conts_df[11:15])
pairs(conts_df[16:20])
pairs(conts_df[21:25])

# 1) there appears to be a strong linear relationship between MTGBAL and CCBAl
# numerically investigate the relationship between MTGBAL and CCBAL
cor(ins_t$MTGBAL, ins_t$CCBAL, method = "pearson", use = "complete.obs")

# 2) number of teller visit interactions drops sharply as total ATM withdrawal amount increases
# 3) number of checks written appears to drop as number of telephone banking interactions increases
# 4) exactly the same number of missing values for eight variables
# 5) age variable has the highest number of missing values (perhaps customers uncomfortable reporting this?)


# TODO: closely examine the response variable (INS), use summary function, plot against other predictor, etc...

# use Boolean indexing to determine the number of customers that purchased an insurance product
ins_purch <- ins_t$INS[ins_t$INS == 1]
num_ins_purch <- length(ins_purch)

# use Boolean indexing to determine the number of customers that did not purchase an insurance product
no_purch <- ins_t$INS[ins_t$INS != 1]
num_no_purch <- length(no_purch)

# do the same thing as above, all while staying in the tidyverse
purchases <- ins_t %>%
  count(INS, sort = T)

# use box plots to visualize the relationship between INS and continuous predictors
ggplot(ins_t) +
  geom_boxplot(aes(x = as.factor(INS), y = CRSCORE))

ggplot(ins_t) +
  geom_boxplot(aes(x = as.factor(INS), y = INCOME))

ggplot(ins_t) +
  geom_boxplot(aes(x = as.factor(INS), y = CCBAL))


# subset data frame to remove observations where INV is missing
ins_t2 <- ins_t %>%
  filter(!is.na(INV))

# use cross-tabulation tables to visualize the relationship between INS and categorical predictors
ggplot(data = ins_t2) +
  geom_bar(mapping = aes(x = as.factor(INV), fill = as.factor(INS)))
