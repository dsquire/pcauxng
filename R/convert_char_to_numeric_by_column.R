
# ****************************************************************************
#          This function takes as input a data frame that has a mix
#           of data types, such as nominal, ordinal and continuous.
#           If a particular column is found to contain character data
#           (e.g. T,F, or A,B,C), and if the values are found to be
#           factor data, the data will be converted to numeric data
# ****************************************************************************

convert_char_to_numeric_by_column <- function(df) {
  
  # BEGiN ChatGPT
  # Identify character or factor columns (ignoring the first row)
  char_factor_cols <- sapply(df[-1, ], function(col) is.character(col) || is.factor(col))
  
  # Convert identified columns to numeric, ignoring the first row
  df[-1, char_factor_cols] <- lapply(df[-1, char_factor_cols], function(col) as.numeric(as.factor(col)))
  
  # END ChatGPT
  return(df)
}

generate_test_dataframe <- function(n_nominal, n_ordinal, n_continuous, n_rows) {
  # Initialize an empty list to store the columns
  columns <- list()
  
  # Generate nominal columns
  for (i in 1:n_nominal) {
    # Randomly assign nominal categories (e.g., 3 categories: "A", "B", "C")
    categories <- sample(LETTERS[1:3], n_rows - 1, replace = TRUE)
    col_name <- paste0("nominal_", i)
    columns[[col_name]] <- c("nominal", categories)  # First row is the label
  }
  
  # Generate ordinal columns
  for (i in 1:n_ordinal) {
    # Randomly assign ordinal values (e.g., 1, 2, 3 representing ordered categories)
    ordinals <- sample(1:5, n_rows - 1, replace = TRUE)
    col_name <- paste0("ordinal_", i)
    columns[[col_name]] <- c("ordinal", ordinals)  # First row is the label
  }
  
  # Generate continuous columns
  for (i in 1:n_continuous) {
    # Randomly assign continuous values (e.g., from a normal distribution)
    continuous <- round(rnorm(n_rows - 1, mean = 0, sd = 1), 2)
    col_name <- paste0("continuous_", i)
    columns[[col_name]] <- c("continuous", continuous)  # First row is the label
  }
  
  # Combine all columns into a dataframe
  df <- as.data.frame(columns)
  
  return(df)
}

options(error = recover)

# Example usage
set.seed(128)  # For reproducibility
test_df <- generate_test_dataframe(n_nominal = 2, n_ordinal = 2, n_continuous = 2, n_rows = 10)
print(test_df)

converted_df <- convert_char_to_numeric_by_column(test_df)
print(converted_df)



