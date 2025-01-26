
# ****************************************************************************
#          This function takes as input a data frame that has a mix
#           of data types, such as nominal, ordinal and continuous.
#           If a particular column is found to contain character data
#           (e.g. T,F, or A,B,C), and if the values are found to be
#           factor data, the data will be converted to numeric data.
#                                                     BC 1/25/2025
# ****************************************************************************

convert_char_to_numeric_by_column <- function(df) {
  
  # BEGiN ChatGPT
  df[] <- lapply(df, function(column) {
    if (is.character(column)) {
      as.numeric(factor(column, levels = unique(column)))
    } else {
      column
    }
  })
  return(df)
  # END ChatGPT
}


# *******************************************************************************
#
#   All syntax below this line is for prototyping purposes and will not be 
#   included in the final version of PcAuxNG
#
# *******************************************************************************
generate_test_dataframe <- function(n_nominal, n_ordinal, n_continuous, n_rows) {
  # Initialize an empty list to store the columns
  columns <- list()
  
  # Generate nominal columns
  for (i in 1:n_nominal) {
    # Randomly assign nominal categories (e.g., 3 categories: "A", "B", "C")
    categories <- sample(LETTERS[1:3], n_rows, replace = TRUE)
    col_name <- paste0("nominal_", i)
    columns[[col_name]] <- c(categories)  
  }
  
  # Generate ordinal columns
  for (i in 1:n_ordinal) {
    # Randomly assign ordinal values (e.g., 1, 2, 3 representing ordered categories)
    ordinals <- sample(1:5, n_rows, replace = TRUE)
    col_name <- paste0("ordinal_", i)
    columns[[col_name]] <- c(ordinals)  
  }
  
  # Generate continuous columns
  for (i in 1:n_continuous) {
    # Randomly assign continuous values (e.g., from a normal distribution)
    continuous <- round(rnorm(n_rows, mean = 0, sd = 1), 2)
    col_name <- paste0("continuous_", i)
    columns[[col_name]] <- c(continuous)  
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



