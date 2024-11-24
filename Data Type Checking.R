# Function to get data types of columns, handling mixed data types
get_data_types <- function(df) {
  result <- list()

  for (col in names(df)) {
    column <- df[[col]]

    # Check if all elements have the same data type
    unique_data_types <- unique(sapply(column, class))

    if (length(unique_data_types) == 1) {
      result[[col]] <- class(column[[1]])
    } else {
      result[[col]] <- "mixed"
    }
  }

  return(result)
}




# Install and load necessary packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}
library(dplyr)
library(purrr)

# Function to generate a data frame with random values of different data types
generate_random_data_frame <- function(num_rows, num_cols) {
  set.seed(123)  # Set seed for reproducibility

  # Define data types
  data_types <- c("numeric", "character", "logical", "factor", "integer")

  # Sample data types for each column
  column_data_types <- sample(data_types, num_cols, replace = TRUE)

  # Generate random values for each column based on data types
  columns <- map2(column_data_types, seq_len(num_cols), function(data_type, col_num) {
    switch(
      data_type,
      numeric = runif(num_rows),
      character = sample(letters, num_rows, replace = TRUE),
      logical = sample(c(TRUE, FALSE), num_rows, replace = TRUE),
      factor = factor(sample(c("A", "B", "C"), num_rows, replace = TRUE)),
      integer = sample(seq(-10, 10), num_rows, replace = TRUE)
    )
  })

  # Create the data frame
  result_df <- as.data.frame(setNames(columns, paste0("Column_", seq_len(num_cols))))

  return(result_df)
}


# Example usage:
num_rows <- 5
num_cols <- 4

# Generate a data frame with random values of different data types
random_data_frame <- generate_random_data_frame(num_rows, num_cols)

# Display the generated data frame
print("Generated Data Frame:")
print(random_data_frame)


# Get data types of columns, handling mixed data types
data_types_result <- get_data_types(my_df)

# Display the result
print("Data Types of Columns:")
print(data_types_result)
