### Variable Selection

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework2_LR/data")
# read in the data
train_bin <- read.csv("insurance_t_bin.csv")

# display full decimal value instead of using scientific notation
options(scipen = 999)

# import libraries
library(ggplot2)
library(tidyverse)
library(gmodels)
library(DescTools)

# options(max.print = F)


# Part 1: Impute missing values and check for linear separation

# Identify variables with missing values
colSums(is.na(train_bin))

# INV, CC, CCPURC, HMOWN have missing values
# impute missing values for INV variable
train_bin <- train_bin %>%
  mutate("INV" = ifelse(is.na(INV), "M", INV))
# impute missing values for CC variable
train_bin <- train_bin %>%
  mutate("CC" = ifelse(is.na(CC), "M", CC))
# impute missing values for CCPURC variable
train_bin <- train_bin %>%
  mutate("CCPURC" = ifelse(is.na(CCPURC), "M", CCPURC))
# impute missing values for HMOWN variable
train_bin <- train_bin %>%
  mutate("HMOWN" = ifelse(is.na(HMOWN), "M", HMOWN))
# check NAs again
colSums(is.na(train_bin))

new_df <- train_bin %>%
  filter(is.na(CC))

new_df2 <- train_bin %>%
  filter(!is.na(CC))


# Check each variable for separation concerns
for (i in 1:length(train_bin)){
  tab <- table(train_bin[,i],train_bin[,21])
  if (0 %in% tab){
    print(tab)
    print(names(train_bin)[i])
  }
}

# CASHBK and MMCRED have quasi complete separation. INS is the target variable so ignore that.

# Combine levels to fix separation concerns for CASHBK and MMCRED

# CASHBK
train_bin <- train_bin %>% 
  mutate(CASHBK = ifelse(CASHBK >= 1, "1+", as.character(CASHBK)))
# MMCRED
train_bin <- train_bin %>% 
  mutate(MMCRED = ifelse(MMCRED > 2, "3+", as.character(MMCRED)))

# Run loop to check for separation again
for (i in 1:length(train_bin)){
  tab <- table(train_bin[,i],train_bin[,21])
  if (0 %in% tab){
    print(tab)
    print(names(train_bin)[i])
  }
}

# No separation (INS) is target variable


## Part 2: Build a main effects only binary logistic regression model

# convert all variables to factors
col_names <- names(train_bin)
train_bin[,col_names] <- lapply(train_bin[,col_names], factor)

# perform a backward selection to determine a main effects logistic regression model
full_model <- glm(INS ~ ., data = train_bin, family = binomial(link = "logit"))
empty_model <- glm(INS ~ 1, data = train_bin, family = binomial(link = "logit"))

# step 1: separate the wheat from the chaff
alpha_f <- 0.002
back_model <- step(full_model,
                   scope = list(lower = empty_model,
                                upper = full_model),
                   direction = "backward", k = qchisq(alpha_f, 1, lower.tail = FALSE))

## Methodology: For our main effects model selection, we used a BIC selection criteria as 
## the BIC criteria automatically adjusts the alpha level used to determine significance of
## variables to be added to the model based on sample size. By imposing this stricter selection
## criteria, we hope to avoid frivolous predictors being added to the main effects model. This
## selection criteria is also most appropriate since we wish to provide an explanatory model
## vs. a predictive model.


# Part 3: Interpret odds ratios for variables in the final model

# create a list of binary predictors in model and the response
bvars <- c("INS", "NSF", "MTG", "ILS", "IRA", "DDA", "MM")
# subset data frame to include only binary variables in model
df_bvars <- train_bin %>%
  select(bvars)
# create an empty data frame to fill
df_odds <- data.frame(variable = character(), odds_ratio = double())

# calculate odds ratio for each each binary predictor in relation to response
for (i in 1:length(colnames(df_bvars)) - 1) {
  # populate data frame
  df_odds[i, "odds_ratio"] <- OddsRatio(table(df_bvars[,1], df_bvars[,(i+1)]))
  df_odds[i, "variable"] <- colnames(df_bvars)[i+1]
}

# The largest odds ratio detected was between INS and the IRA predictor
# with an odds ratio of 3.18, we can interpret this as meaning that 
# customers with an IRA are 3.18 times as likely to purchase an annuity
# as customers who do not have an IRA.

# As was observed in part 1, it appears that customers who are focused
# on long-term investing are (perhaps intuitively) those same customers
# who would be more likely to purchase an insurance product.


# Part 4: Investigate possible interactions using forward selection

# create a data frame of all variables in the model
vars <- c("INS", "DDA", "NSF", "IRA", "INV", "ILS", "MM", "MTG",
          "CC", "DDABAL_Bin", "CHECKS_Bin", "TELLER_Bin", "SAVBAL_Bin",
          "ATMAMT_Bin", "CDBAL_Bin")
df_vars <- train_bin %>%
  select(vars)

# create a main effects model (initial variables chosen from backwards selection)
main_model <- glm(INS ~ ., data = df_vars,
                   family = binomial(link = "logit"))

# create an interaction model (create interactions from best variables)
int_model <- glm(INS ~ . + (.)^2, data = df_vars,
                 family = binomial(link = "logit"))

# start at model with best variables and check all interactions between these variables
final_model <- step(main_model,
                  scope = list(lower = formula(main_model),
                               upper = formula(int_model)),
                  direction = "forward", k = log(nrow(df_vars)))

# Use Likelihood Ratio Test to rank the variables in the final model by significance

# dda variable
dda_model <- glm(INS ~ NSF + IRA + INV + ILS + MM + MTG + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
dda_aov <- anova(back_model, dda_model, test = 'LRT')
print(dda_aov)

# nsf variable
nsf_model <- glm(INS ~ DDA + IRA + INV + ILS + MM + MTG + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
nsf_aov <- anova(back_model, nsf_model, test = 'LRT')
print(nsf_aov)

# ira variable
ira_model <- glm(INS ~ DDA + NSF + INV + ILS + MM + MTG + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
ira_aov <- anova(back_model, ira_model, test = 'LRT')
print(ira_aov)

# inv variable
inv_model <- glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
inv_aov <- anova(back_model, inv_model, test = 'LRT')
print(inv_aov)

# ils variable
ils_model <- glm(INS ~ DDA + NSF + IRA + INV + MM + MTG + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
ils_aov <- anova(back_model, ils_model, test = 'LRT')
print(ils_aov)

# mm variable
mm_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MTG + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
mm_aov <- anova(back_model, mm_model, test = 'LRT')
print(mm_aov)

# mtg variable
mtg_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + CC + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
mtg_aov <- anova(back_model, mtg_model, test = 'LRT')
print(mtg_aov)

# cc variable
cc_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + DDABAL_Bin + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
cc_aov <- anova(back_model, cc_model, test = 'LRT')
print(cc_aov)

# ddabal_bin variable
ddabal_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + CC + 
                   CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
ddabal_aov <- anova(back_model, ddabal_model, test = 'LRT')
print(ddabal_aov)

# checks_bin variable
checks_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + CC + 
                   DDABAL_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
checks_aov <- anova(back_model, checks_model, test = 'LRT')
print(checks_aov)

# teller_bin variable
teller_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + CC + 
                   DDABAL_Bin + CHECKS_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
teller_aov <- anova(back_model, teller_model, test = 'LRT')
print(teller_aov)

# savbal_bin variable
savbal_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + CC + 
                   DDABAL_Bin + CHECKS_Bin + TELLER_Bin + ATMAMT_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
savbal_aov <- anova(back_model, savbal_model, test = 'LRT')
print(savbal_aov)

# atmamt_bin variable
atmamt_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + CC + 
                   DDABAL_Bin + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + CDBAL_Bin,
                 data = train_bin, family = binomial(link = "logit"))
atmamt_aov <- anova(back_model, atmamt_model, test = 'LRT')
print(atmamt_aov)

# cdbal_bin variable
cdbal_model <- glm(INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + CC + 
                   DDABAL_Bin + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin,
                 data = train_bin, family = binomial(link = "logit"))
cdbal_aov <- anova(back_model, cdbal_model, test = 'LRT')
print(cdbal_aov)

# interaction variable
int_aov <- anova(back_model, final_model, test = "LRT")
print(int_aov)
