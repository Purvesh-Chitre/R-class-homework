#############################################################
# HOMEWORK 5
## 22/03/2023
#############################################################

# creating data for dataframe
weight <- c(30, 25, 150, 300, 45, 320)
length <- c(120, 130, 520, 480, 140, 540)
width <- c(70, 82, 170, 185, 90, 200)
classes <- c("bird", "bird", "mammal", "mammal", "bird", "mammal")

# creating the dataframe using cbind
animal_data_frame <- cbind(weight, length, width, classes)

##############################################################
### SIMPLE RAMDOM SAMPLING
##############################################################

# creating training data using random sample
training_index <- sample(1: nrow(animal_data_frame), size = 4)

# creating training dataframe
trainin_dataframe <- animal_data_frame[training_index, ]

# creating testing dataframe
testing_dataframe <- animal_data_frame[-training_index, ]

##############################################################
### STRATIFIED SAMPLING
##############################################################

# calling the library
library(splitstackshape)

# creating train data with stratified function
strat_train <- stratified(as.data.frame(animal_data_frame), group = 4, size = 0.6)

# creating testing data with stratified dunction and bothsets as TRUE
training_testing <- stratified(as.data.frame(animal_data_frame), group = 4, size = 0.6, bothSets = T)

training_testing$SAMP1 # stratified training data
training_testing$SAMP2 # stratified testing data

