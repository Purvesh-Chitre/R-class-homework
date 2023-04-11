#####################################################
#################### HOMEWORK 7 #####################
#################### 27/03/2023 #####################
###################### DDMBAN #######################
############# pchitre@student.hult.edu ##############
#####################################################

# loading the library
library(mlbench)

# calling the data
data("PimaIndiansDiabetes")

# creating a small dataframe
my_df <- PimaIndiansDiabetes

# creating the second small dataframe
my_df2 <- my_df

# converting vector to a character data in diabetes column
my_df2$diabetes <- as.character(my_df2$diabetes)

# convert positive to 1 and negarive to 0 with gsub fucntion
my_df2$diabetes <- gsub("neg", "0", my_df2$diabetes)
my_df2$diabetes <- gsub("pos", "1", my_df2$diabetes)

# convert character data to numeric for diabetes column
my_df2$diabetes <- as.numeric(my_df2$diabetes)

# making the tree
my_tree <- rpart(diabetes ~ pregnant + pressure + age + mass + triceps + pedigree,
                 data = my_df2, method = "class", 
                 control = rpart.control(minsplit = 20,
                                         minbucket = 15,
                                         cp = 0.01))

# plotting the tree
rpart.plot(my_tree, type = 2, extra = 2)
