# ***************************************************************************
#
# This function creates an extinction vector and pre-populates it with zeros.
#
# ***************************************************************************
prepare_extinction_vec <- function(df){
  
  extinction_vec <- c()
  n <- ncol(df)
  for(i in 1:n){
    extinction_vec <- c(extinction_vec, 0)
  }
  return(extinction_vec)
}



create_dataframe <- function(num_rows, num_columns) {
  # Initialize an empty list to store columns
  columns <- vector("list", num_columns)
  col_names <- paste("Column", 1:num_columns, sep = "_")  # Name columns
  
  # Loop through each column, assigning a random data type
  for (i in 1:num_columns) {
    # Randomly choose a data type for the column
    data_type <- sample(c("numeric", "character", "logical", "factor"), 1)
    
    # Create column based on the chosen data type
    if (data_type == "numeric") {
      columns[[i]] <- rnorm(num_rows)  # Numeric column with random values
    } else if (data_type == "character") {
      columns[[i]] <- sample(LETTERS, num_rows, replace = TRUE)  # Character column with random letters
    } else if (data_type == "logical") {
      columns[[i]] <- sample(c(TRUE, FALSE), num_rows, replace = TRUE)  # Logical column with TRUE/FALSE values
    } else if (data_type == "factor") {
      columns[[i]] <- factor(sample(c("Low", "Medium", "High"), num_rows, replace = TRUE))  # Factor column
    }
  }
  
  # Combine columns into a data frame
  my_dataframe <- data.frame(columns)
  names(my_dataframe) <- col_names  # Assign column names
  
  return(my_dataframe)
}

# Example: create a data frame with 5 rows and 3 columns
df <- create_dataframe(3, 5)
print(df)

extinction_vec <- prepare_extinction_vec(df)
print(extinction_vec)

tempVar <- extinction_vec[2]
tempVar