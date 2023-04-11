####################################

######### MBANDD SFO 2023 ##########
########### Author: PPC ############
######## date: 03/19/2023 ##########
#### pchitre@student.hult.edu ######

####################################

## importing the MASS library
# install.packages("MASS")

## checcking for the library MASS
library(MASS)

## Checking for the airquality dataset
airquality

# loading the airquality dataset in the Global Environment
data(airquality)

# 1. For the matrix created in Exercise 7 from chapter 4:
# a) Calculate column medians using the apply() function.
apply(airquality, 2, median)
# b) Calculate row medians using the apply() function.
apply(airquality, 1, mean)

# 2. For the matrix that was given as an example in chapter 2.1:
# a) Calculate row medians using the apply() function.
apply(airquality, 1, median)
# b) Calculate column means using the apply() function.
apply(airquality, 2, mean)

# 10. Create a user defined function that can clean up almost any
#     data frame using a loop. The function inputs are the dataset name
#     and the column indexes that we want to clean up. An example of
#     the function call might look like this:
#     function_name(x=mydata, col_idx=c(1,2,3))
#     The loop inside the function will take column indexes (from the
#     call inputs) and remove observations with empty values.
#     Hint: inside the loop, use:
#     new_df <- x[-which(is.na(x[ ,col_idx[i]]))]
#     Test your function with any dataset found on the
#     www.images.aqr.com/Insights/Datasets website.

# Define a function to clean a data frame by removing rows with missing values in specified columns
clean_data <- function(x, col_idx) {
  # Create a new data frame to store cleaned data
  new_df <- x
  # Loop over specified columns
  for (i in col_idx) {
    # Remove rows with missing values in the current column
    new_df <- new_df[which(!is.na(new_df[,i])),]
  }
  # Return the cleaned data frame
  return(new_df)
}

# Load the airquality dataset
data(airquality)

# Clean the airquality dataset by removing rows with missing values in columns 1, 2, and 3
cleaned_airquality <- clean_data(airquality, c(1,2,3))

# View the first few rows of the cleaned dataset
head(cleaned_airquality)


