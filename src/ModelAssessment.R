### Model Assessment

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Logistic Regression/Homework3_LR/data")
# read in the data
train_bin <- read.csv("insurance_t_bin.csv")

# display full decimal value instead of using scientific notation
options(scipen = 999)

# import libraries
library(ggplot2)
library(tidyverse)
library(gmodels)
library(DescTools)
library(InformationValue)
library(ROCR)


## Part 1: Recreate the model from homework 2

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

# Combine levels to fix separation concerns for CASHBK and MMCRED

# CASHBK
train_bin <- train_bin %>% 
  mutate(CASHBK = ifelse(CASHBK >= 1, "1+", as.character(CASHBK)))
# MMCRED
train_bin <- train_bin %>% 
  mutate(MMCRED = ifelse(MMCRED > 2, "3+", as.character(MMCRED)))

# convert all variables to factors
col_names <- names(train_bin)
train_bin[,col_names] <- lapply(train_bin[,col_names], factor)

# create a data frame of all variables in the model
vars <- c("INS", "DDA", "NSF", "IRA", "INV", "ILS", "MM", "MTG",
          "CC", "DDABAL_Bin", "CHECKS_Bin", "TELLER_Bin", "SAVBAL_Bin",
          "ATMAMT_Bin", "CDBAL_Bin")

# create a data frame of only variables in final model
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


## Part 2: Calculate probability metrics on training data

# calculate concordance percentage on the training data
Concordance(train_bin$INS, predict(final_model, type = "response"))

# calculate coefficient of discrimination (discrimination slope) on training data

# generate predicted probabilities on training data
df_vars$p_hat <- predict(final_model, type = "response")
# filter all predicted probabilities for events
p1 <- df_vars$p_hat[df_vars$INS == 1]
# filter all predicted probabilities for nonevents
p0 <- df_vars$p_hat[df_vars$INS == 0]
# use these to calculate the coefficient of discrimination
coef_discrim <- mean(p1) - mean(p0)

# use histograms to create a visual representation
ggplot(df_vars, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5))
  

## Part 3: Calculate classification metrics on training data

# visually show the ROC curve
plotROC(df_vars$INS, df_vars$p_hat)

# calculate the K-S statistic to determine threshold for classification

# create a prediction object
pred <- prediction(fitted(final_model), factor(df_vars$INS))
# use the performance function to calculate the true positive and false positive rates
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# calculate K-S statistic (the maximum distance between the two CDFs)
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
# calculate the cutoff value that yields this KS statistic
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
# print these values to the console
print(c(KS, cutoffAtKS))


## Part 4: Calculate classification metrics on validation data

# read in the validation data
val_bin <- read.csv("insurance_v_bin.csv")

# impute missing values for INV variable
val_bin <- val_bin %>%
  mutate("INV" = ifelse(is.na(INV), "M", INV))
# impute missing values for CC variable
val_bin <- val_bin %>%
  mutate("CC" = ifelse(is.na(CC), "M", CC))
# impute missing values for CCPURC variable
val_bin <- val_bin %>%
  mutate("CCPURC" = ifelse(is.na(CCPURC), "M", CCPURC))
# impute missing values for HMOWN variable
val_bin <- val_bin %>%
  mutate("HMOWN" = ifelse(is.na(HMOWN), "M", HMOWN))

# Combine levels to fix separation concerns for CASHBK and MMCRED (no need to check for separation concerns in validation data)
# CASHBK
val_bin <- val_bin %>% 
  mutate(CASHBK = ifelse(CASHBK >= 1, "1+", as.character(CASHBK)))
# MMCRED
val_bin <- val_bin %>% 
  mutate(MMCRED = ifelse(MMCRED > 2, "3+", as.character(MMCRED)))

# convert all variables to factors
val_col_names <- names(val_bin)
val_bin[,val_col_names] <- lapply(val_bin[,val_col_names], factor)

# create a data frame of only variables in final model
df_val_vars <- val_bin %>%
  select(vars)

# use the model built on the training data to obtain predictions for validation data
df_val_vars$p_hat <- predict(final_model, newdata = val_bin, type = "response")

# create a column of classifications by applying threshold to prediction values
df_val_vars <- df_val_vars %>%
  mutate(y_hat = ifelse(p_hat > cutoffAtKS, 1, 0))

# create a column of true positives
df_val_vars <- df_val_vars %>%
  mutate(TP = ifelse((INS == 1 & y_hat == 1), 1, 0))

# create a column of true negatives
df_val_vars <- df_val_vars %>%
  mutate(TN = ifelse((INS == 0 & y_hat == 0), 1, 0))

# calculate model accuracy ((TP + TN) / n)
accuracy = (sum(df_val_vars$TP) + sum(df_val_vars$TN)) / nrow(df_val_vars)

# display the final confusion matrix
confusionMatrix(df_val_vars$INS, df_val_vars$p_hat, threshold = cutoffAtKS)
# display the final confusion matrix using y_hat column
confusionMatrix(df_val_vars$INS, df_val_vars$y_hat)

# generate a lift curve to help show model performance
pred_val <- prediction(df_val_vars$p_hat, factor(df_val_vars$INS))
perf_lift <- performance(pred_val, measure = "lift", x.measure = "rpp")
plot(perf_lift, lwd = 3, colorize = TRUE, colorkey = TRUE, 
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Validation Data")
abline(h = 1, lty = 3)
