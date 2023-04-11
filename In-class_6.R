#############################################
############# MBANDD SFO 2023 ###############
############### Author: PPC #################
############# date: 03/28/2023 ##############
######### pchitre@student.hult.edu ##########
################ IN-CLASS 6 #################
################ 03/23/2023 #################
#############################################

## importing the library readxl 
library(readxl)
ger_ccard <- read_excel("Documents/MBAN/Visualizing & Analyzing Data with R-Methods & Tools/german credit card-1.xls")
View(ger_ccard) 

### let's create a few data objects

## creating a customer id list
cust_list <- nrow(ger_ccard)

## number of variable list
var_list <- ncol(ger_ccard)

## let's create vectors
dim_vector <- c(cust_list, var_list) ## this is to have order of rows and columns

## root out specific vectors due to potentially same issues
var1_list <- ger_ccard$purpose
var2_list <- ger_ccard$good_bad
## created to identify object to fix issues
## created a relationship between different objects for a given 
## sample of population

## creating a matrix of the 2 above variable list
matrix1 <- matrix(c(var1_list, var2_list), nrow = cust_list, ncol = 2)

## creating dataframe with the matrix
df_matrix1 <- as.data.frame(matrix1)

## getting list in the required format
ger_list <- list(ger_ccard, matrix1, var1_list, var2_list, cust_list)
# categorizing data per business unit, a common denominator or a common theme
# this is the information gain, by grouping, classifying.

# subsetting aka information loss process

var1_list[2:5]

## going from a vector to a scalar

dim_vector[1] # index for observations
dim_vector[2] # index for measurements

## going from matrix to a scalar
# breaking down the matrix, using all the observations
# investigating one at a time
matrix1[ , 1] # just investigating the purpose column

matrix1[ , 2] # investigating the good_bad column

## getting the frequency data
table(matrix1[ , 1]) # using the table function to get count of discrete values

table(matrix1[ , 2]) # get the count for the good_bad variable
# information loss process helped gain this information for the business case
# insights through targeted specific variable selection.
# by doing detailed questioning for one aspect, it is information loss to gain
# more information

# trying to fix problem with purpose and good_bad
is.character(ger_ccard$purpose) # understanding the data in purpose
is.character(ger_ccard$good_bad) # understanding the data in good_bad
# this just checked the data and not fix it

as.numeric(ger_ccard$purpose) # converting the purpose variable to numeric
# but it is not converting effectively

as.numeric(ger_ccard$good_bad) # converting the good_bad variable to numeric

## R does not know the business logic to conver the character data to logic
# in casse of the purpose where we had 'X' which was possibly a 10, but the 
# data collection did not maybe allow for the data to be entered as 10. It
# could also be 7 as the frequency is low and close to 9 observations of 8.
# Hence, it makes sense at this point to ask the business stakeholder 
# that provided the data on the clarification for the use of 'X' in the data
# to establish it's significance.

## information loss process based on business logic
# using the which function
which(ger_ccard$age > 40 & ger_ccard$amount > 9000) # example

# checking for observations which have X in purpose
which(ger_ccard$purpose == "X") 

ger_ccard[ which(ger_ccard$purpose == "X") , ]

## sub-setting the good_bad into 2 subsets
good_subset <- ger_ccard[ which(ger_ccard$good_bad == "good") , ]

# summary for the good_subset
summary(good_subset)

bad_subset <- ger_ccard[ which(ger_ccard$good_bad == "bad") , ]

# summary for the bad_subset
summary(bad_subset)

# gsub() function to replace elements in the variable with a different value
## replacing X with 10 and good_bad with 1 and 0
# gsub("replace what", "with", "where")

# fixing the purpose data and inputting it to a new attribute/variable
ger_ccard$purpose_fixed <- as.numeric(gsub("X", "10", ger_ccard$purpose))

table(ger_ccard$purpose_fixed)

# implementing business logic to good_bad 
# fixing the good_bad data by replacing good with 1 & inputting it to a new 
# attribute/variable
ger_ccard$binary <- gsub("good", "1", ger_ccard$good_bad)

table(ger_ccard$bin)
# fixing the purpose data by replacing good with 1 & inputting it to a new 
# attribute/variable
ger_ccard$binary <- gsub("bad", "0", ger_ccard$binary)
ger_ccard$binary <- as.numeric(ger_ccard$binary)

table(ger_ccard$binary)
# the business logic for using bnary is to predict business success, 
# hence good is 1

## session-4
## investigating descriptive statistics using loops
# horizontal loop uses columns hence ncol, vertical loops use rows hence nrows
# horizontal loop
ger_ccard <- as.data.frame(ger_ccard)

for (i in 1:ncol(ger_ccard)) {
  print(min(ger_ccard[ , i ], na.rm = TRUE))
  print(mean(ger_ccard[ , i ], na.rm = TRUE))
  print(max(ger_ccard[ , i ], na.rm = TRUE))
} # closing the i loop

# Order of the scalars defines the information gain as vectors
# 4 variables which are impacting credit score
# 0.4*Savings+0.3*Checking+0.3*Age+0.0.2*amount

## SCORECARD MODEL - vertical for loop
ger_ccard$risk_score <- c()

# if we use $attribute it is a vector, and hence no , after[i]
for (i in 1:nrow(ger_ccard)) {
  ger_ccard$risk_score[i] <- 0.5*ger_ccard$duration[i] + 
    0.1*ger_ccard$age[i] + 
    0.1*ger_ccard$amount[i] + 
    0.3*ger_ccard$installp[i]
} # closing the risk_score loop

# SCORECARD MODEL-team - vertical for loop
ger_ccard$risk_score_team <- c()

# if we use $attribute it is a vector, and hence no , after[i]
for (i in 1:nrow(ger_ccard)) {
  ger_ccard$risk_score_team[i] <- 0.3*ger_ccard$duration[i] + 
    0.1*ger_ccard$age[i] + 
    0.05*ger_ccard$amount[i] + 
    0.3*ger_ccard$installp[i] +
    0.05*ger_ccard$checking[i]
} # closing the risk_score loop


# implementing if case logic to label customers outstanding

ger_ccard$label <- c()

for (i in 1:nrow(ger_ccard)) {
  if (ger_ccard$good_bad[i] == "good" & ger_ccard$risk_score[i] < 250) {
    ger_ccard$label[i] <- "outstanding"
  }else{
    ger_ccard$label[i] <- "not-outstanding"
  } # closing the if-statement for outstanding
  
} # closing the label for loop with if-statement inside

# checking the table with the labels of outstanding and not-outstanding clients
table(ger_ccard$label)

## SESSION 5
## using UDF
risk_func <- function(var1, w1, var2, w2, var3, w3, 
                      var4, w4){
  my_score <- (var1*w1 + var2*w2 + var3*w3 + var4*w4)
  return(my_score)
} # closing the risk_func

# need to call risk_func UDF
# checking for team 3 score weights
ger_ccard$t3_score <- risk_func(var1 = ger_ccard$history, w1 = 0.15,
                                var2 = ger_ccard$age, w2 = 0.2,
                                var3 = ger_ccard$amount, w3 = 0.3,
                                var4 = ger_ccard$installp, w4 = 0.35)

# checking for team 2 score weights
ger_ccard$t2_score <- risk_func(var1 = ger_ccard$history, w1 = 0.25,
                                var2 = ger_ccard$age, w2 = 0.3,
                                var3 = ger_ccard$amount, w3 = 0.25,
                                var4 = ger_ccard$installp, w4 = 0.2)

# calculating mu and Sigma for all the vars in the data
# data scientists use this to optimization
# creating new vector of min, max, mu and sigma with for loop

for (i in 1:ncol(ger_ccard)) {
  my_min <- try(min(ger_ccard[ , i], na.rm = TRUE))
  my_max <- try(max(ger_ccard[ , i], na.rm = TRUE))
  my_mu <- try(mean(ger_ccard[ , i], na.rm = TRUE))
  my_std <- try(sd(ger_ccard[ , i], na.rm = TRUE))
  print(c(my_min, my_mu, my_std, my_max))
} # closing the for loop for desc stats
# this is used to gain information, heterogeneous because they all have
# different business units.
# heterogeneous variables and hence the relation is difficult to establish 
# between 2 different unit of measures or business units.

team_dice_rolls <- c(6,4, 1,5, 6,1, 1,1, 6,1, 3,1, 6,6, 4,4, 4,3, 
                     1,2, 2,6, 4,4, 4,4, 3,4, 6,3, 4,3, 1,5)
mean(team_dice_rolls)

# uniform, binomial, and exponential distribution of data
# codifying the casino game ::
# dice
dice <- sample(c(1,2,3,4,5,6), size = 10000, replace = TRUE)

# for a dice the mean would be 3.5 or close to it
mean(dice)

hist(dice) # uniform distribution

team_coin_flip <- c(0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,0,1,1,0,1)
mean(team_coin_flip)

# binomial distribution - coin flip

coin_flip <- sample(c(0,1), size = 60000, replace = TRUE)

mean(coin_flip)

hist(coin_flip)

# exponential distribution - how much time has passed between successful event
# var = 1/lambda^2
# mean = 1/lambda
# rexp() function

trains <- rexp(10000, rate = 0.5)
hist(trains)

# lambda = c(0.5, 1, 3, 10)

for (i in 1:ncol(ger_ccard)) {
  try(hist(ger_ccard[ , i ]))
} # closing the for loop
# gives various distribution patterns of histograms

## SESSION - 6
# normalization and standardization is used to remove business units to
# make all the values comparable to each other
# creating a UDF to standardize
# calculation the z-scores (z-test)
# z-score = (x-mean(x))/sd(x)

standard_z <- function(x){ # "x" is the variable in ger_ccard dataset
  temp_x <- (x-mean(x))/sd(x)
  return(temp_x) # is a vector
} # closing the standardize function

ger_ccard$checking_standard <- standard_z(x=ger_ccard$checking)
ger_ccard$age_standard <- standard_z(x=ger_ccard$age)
ger_ccard$amountg_standard <- standard_z(x=ger_ccard$amount)

# now as these do not have business units so we can add them, 
# we can average, can calcualte a weighted average
# them which allows for better feature engineering
# z score is hard to interpret, hence we use d t-score (t-test)
# a t-score is better because it is a standardized z score.
# t-score = (z-score * 10) + 50
# t-scores look like percentiles, easier to interpret

# normalization - rescaling values to a range between [0,1] in stastics
# xnew = (xi - min(x))/(max(x)-min(x))

# creating UDF to normalization
normalize_udf <- function(x){ # "x" is the variable in ger_ccard dataset
  temp_scale <- (x-min(x))/(max(x)-min(x))
  return(temp_scale)
} # closing the normalize function

ger_ccard$checking_norm <- normalize_udf(x = ger_ccard$checking)
summary(ger_ccard$checking_norm)
summary(ger_ccard$checking)
# from the result, we can see min as 0.0000 but it relates to 1 checking acc
# similarly the max which is 1.0000 relates to 4 checking acc
# but the business is a little lost with the mean as there is not business unit

ger_ccard$duration_norm <- normalize_udf(x = ger_ccard$duration)
ger_ccard$history_norm <- normalize_udf(x = ger_ccard$history)
ger_ccard$amount_norm <- normalize_udf(x = ger_ccard$amount)
ger_ccard$savings_norm <- normalize_udf(x = ger_ccard$savings)
ger_ccard$installp_norm <- normalize_udf(x = ger_ccard$installp)
ger_ccard$coapp_norm <- normalize_udf(x = ger_ccard$coapp)
ger_ccard$age_norm <- normalize_udf(x = ger_ccard$age)
ger_ccard$t3_score_norm <- normalize_udf(x = ger_ccard$t3_score)

# we do random sampling to increase efficiency, remove biases, cross-validation,
# training and testing, avoid over fitting
# when data is heterogeneous you take a larger sample for training
# when homogeneous data, we use smaller sampling for training

# Step 1
# random sampling by using sample() function
training_idx <- sample(1: nrow(ger_ccard), size = 0.8*nrow(ger_ccard) )

# Step 2
# setting the training and testing datasets
ger_ccard_train <- ger_ccard[ training_idx , ]
ger_ccard_test <- ger_ccard[ - training_idx, ]

# Step 3
# looking up correlations
# testing a few variables for multicolinearity 
cor.test(ger_ccard$age, ger_ccard$amount)
cor.test(ger_ccard$age, ger_ccard$duration)
cor.test(ger_ccard$duration, ger_ccard$amount)

# logistic regression provides the ODDS of success,
# it provides a classification of 1 & 0. Giving a probability,
# thus a regression to understand causation. It differs from the liner
# regression by exponential, that is the use of natural log in its calculations

###################################################
## SESSION 7
## 04/03/2023
###################################################
exp(0.2)-1
exp(-0.08)-1
exp(0.9)-1

# logistic regression for Deutsche bank
ger_ccard_logit <- glm(binary ~ age + duration + checking + coapp + amount + history,
    data = ger_ccard_train, family = "binomial")
summary(ger_ccard_logit)

exp(-0.03283)-1

# on logistic regression we do not analyse intercept
exp(0.71)-1
exp(0.3921)-1
# business insight in terms of business odds of success needs to be considered 
# individually and not collectively, one at a time considering others are constants

# normalize data to compare collectively
ger_ccard_logit_norm <- glm(binary ~ age_norm + duration_norm + checking_norm + coapp_norm + amount_norm + history_norm,
                       data = ger_ccard_train, family = "binomial")
summary(ger_ccard_logit_norm)
exp(0.8106)-1
# normalizing will allow us to analyze which has strongest impact on business
# success by the strength of the co-efficients. However, we have to go back to 
# our previous data of non-normalized.

# loading the carat library
library(caret)

# using the predict() function
ger_ccard_predict <- predict(ger_ccard_logit, ger_ccard_test, type = "response")

# we use accuracy by use of confusion matrix with the predict data
confusionMatrix(data = as.factor(as.numeric(ger_ccard_predict > 0.5)),
                reference = as.factor(as.numeric(ger_ccard_test$binary)))

# loading the rpart and rpart.plot libraries
library(rpart)
library(rpart.plot)

# using the original logistic model
ger_ccard_tree <- rpart(binary ~ age + duration + checking + coapp + amount + history,
                       data = ger_ccard_train, 
                       method = "class",
                       cp = 0.02)

# we use rpart.plot() function to get the gini tree
rpart.plot(ger_ccard_tree, type = 1, extra = 1)

# creating the pred tree on test data
ger_ccard_tree_pred <- predict(ger_ccard_tree, ger_ccard_test, type = "prob")

# getting the confusion matrix for the test pred tree
confusionMatrix(data = as.factor(as.numeric(ger_ccard_tree_pred[  , 2 ] > 0.5)),
                reference = as.factor(as.numeric(ger_ccard_test$binary)))

