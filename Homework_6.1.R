#############################################
## HOMEWORK 6.1
#############################################

# installing the package
# install.packages("mlbench")

# loading the library
library(mlbench)

# calling the data
data("PimaIndiansDiabetes")

# converting data to numeric
PimaIndiansDiabetes$binary <- as.numeric(PimaIndiansDiabetes$diabetes)-1

# creating training index

train_index <- sample(1:nrow(PimaIndiansDiabetes), 
                      size = 0.8*nrow(PimaIndiansDiabetes))

# creating train data
train_data <- PimaIndiansDiabetes[train_index, ]

# creating testing data
test_data <- PimaIndiansDiabetes[-train_index, ]

# building the model
logit_reg <- glm(binary ~ age+mass+pregnant, 
                 data = train_data, 
                 family = "binomial")

# getting summary
summary(logit_reg)

# change in the odds of success
exp(0.034722)-1
exp(0.097791)-1
exp(0.096804)-1

# probability
predict(logit_reg, test_data, type = "response")

#############################################
## HOMEWORK 6.2
#############################################

# re-scaling age
PimaIndiansDiabetes$res_age <- (PimaIndiansDiabetes$age - min(PimaIndiansDiabetes$age))/
                                (max(PimaIndiansDiabetes$age)-min(PimaIndiansDiabetes$age))

# re-scaling mass
PimaIndiansDiabetes$res_mass <- (PimaIndiansDiabetes$mass - min(PimaIndiansDiabetes$mass))/
                                 (max(PimaIndiansDiabetes$mass)-min(PimaIndiansDiabetes$mass))

# re-scaling pregnant
PimaIndiansDiabetes$res_pregnant <- (PimaIndiansDiabetes$pregnant - min(PimaIndiansDiabetes$pregnant))/
                                     (max(PimaIndiansDiabetes$pregnant)-min(PimaIndiansDiabetes$pregnant))

# summary of the data PimaIndiansDiabetes
summary(PimaIndiansDiabetes)

# creating training index

train_index_res <- sample(1:768, size = 0.8*768)

# creating train data with res
train_data_res <- PimaIndiansDiabetes[train_index_res, ]

# creating testing data with res
test_data_res <- PimaIndiansDiabetes[-train_index_res, ]

# building the model with res
logit_reg_res <- glm(binary ~ res_age+res_mass+res_pregnant, 
                 data = train_data_res, 
                 family = "binomial")

# summary of logit_reg_res
summary(logit_reg_res)
