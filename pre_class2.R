c(101, 102,103, 104, 105, 106)
my_vector <- c(101, 102,103, 104, 105, 106)
my_vector1 <- rep(c(101, 102), each = 3)

my_matrix <- matrix(data = my_vector, nrow = 2, ncol = 3)

my_dataframe <- as.data.frame(my_matrix)