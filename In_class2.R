####################################

######### MBANDD SFO 2023 ##########
########### Author: PPC ############
######## date: 03/13/2023 ##########
#### pchitre@student.hult.edu ######

####################################

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

## subsetting the good_bad into 2 subsets
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

## session-4
## investigating descriptive statistics using loops

# horizontal loop
ger_ccard <- as.data.frame(ger_ccard)

for (i in 1:ncol(ger_ccard)) {
  print(min(ger_ccard[ , i ], na.rm = TRUE))
  print(mean(ger_ccard[ , i ], na.rm = TRUE))
  print(max(ger_ccard[ , i ], na.rm = TRUE))
} # closing the i loop


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
    0.1*ger_ccard$amount[i] + 
    0.3*ger_ccard$installp[i] +
    0.2*ger_ccard$checking[i]
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

table(ger_ccard$label)

