




col1 <- c(  1,   2,   3,    4,    5,   6,   7,    8,    9,   10)
col2 <- c(  1,   2,  NA,  4.3,    5,   6,  NA,  8.1,  9.2,   10)
col3 <- c(  1,  25,  26,   28,   29,  30,  31,   40,   60,   90)
col4 <- c( 10,  20,  30,   40,   50,  60,  70,  200,  300, 1000)
col5 <- c(  1,   2,   2,    1,    3,   3,   2,    1,    1,    2)

get_column_with_more_NA <- function(df, column1, column2) {

  # Count NA values in each specified column
  na_count_col1 <- sum(is.na(df[[column1]]))
  na_count_col2 <- sum(is.na(df[[column2]]))


  # Compare counts and return the index number of the column with more NA values
  if (na_count_col1 > na_count_col2) {
    return(column1)
  } else if (na_count_col2 > na_count_col1) {
    return(column2)
  } else {
    return(0)  # Return 0 if both columns have the same number of NA values
  }
}

modify_vector <- function(input_integer, u) {
  # Check if the input_integer is valid (within the range of the vector)
  if (input_integer <= length(u) & input_integer >= 1) {
    # Set the input_integer-th value of the u_vector to zero
    u[input_integer] <- 0
  } else {
    # Print a warning message if the input_integer is invalid
    print("Invalid input. Please provide an integer within the range of the vector.")
  }

  return(u)
}

count_distinct_values <- function(df) {

  n_columns <- ncol(df)
  vector_of_values <- c()
  for(i in 1:n_columns){
    # Extract the specified column from the data frame
    column <- df[[i]]

    # Get the unique values in the column
    distinct_values <- unique(column)

    # Count the number of distinct values
    num_distinct_values <- length(distinct_values)
    vector_of_values <- append(vector_of_values, num_distinct_values)
  }
  return(vector_of_values)
}

df <- data.frame(col1, col2, col3, col4, col5)
vector_x <- count_distinct_values(df)
print(vector_x)


correl <- cor(df[,1], df[,2], use = "complete.obs")
print(correl)

index <- get_column_with_more_NA(df, 1, 2)
print(index)

num_cols <- ncol(df)
u_vector <- rep(1, num_cols)
u_vector <- modify_vector(index, u_vector)

print(u_vector)
