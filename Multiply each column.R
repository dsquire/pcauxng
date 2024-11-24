# Code segment #1
# This code blindly multiplies every column by every other column

# Example data frame
original_df <- data.frame(
  A = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  B = c(4, 5, 6, 11, 12, 13, 14, 15, 16),
  C = c(7, 8, 9, 11, 22, 33, 44, 55, 66)
)

# Function to multiply every column by every other column
multiply_columns <- function(df) {
  num_columns <- ncol(df)
  new_df <- df  # Create a copy of the original data frame

  # Iterate through each combination of columns
  for (i in 1:num_columns) {
    for (j in 1:num_columns) {
      # Multiply columns i and j and store the result in a new column
      new_col <- df[, i] * df[, j]
      col_name <- paste0(names(df)[i], "_times_", names(df)[j])
      new_df <- cbind(new_df, new_col)
      names(new_df)[ncol(new_df)] <- col_name
    }
  }

  return(new_df)
}

# Call the function with your data frame
result_df <- multiply_columns(original_df)

# Display the original and result data frames
print("Original Data Frame:")
print(original_df)

print("\nResult Data Frame:")
print(result_df)



# Code segment #2
# This code multiplies every column by each column specified in a vector

multiply_columns_function <- function(df, columns_to_multiply = NULL) {
  num_columns <- ncol(df)
  new_df <- df  # Create a copy of the original data frame

  if (is.null(columns_to_multiply)) {
    # If columns_to_multiply is not specified, multiply all columns by all columns
    columns_to_multiply <- 1:num_columns
  }

  # Iterate through each combination of columns
  for (i in columns_to_multiply) {
    for (j in 1:num_columns) {
      # Multiply columns i and j and store the result in a new column
      new_col <- df[, i] * df[, j]
      col_name <- paste0(names(df)[i], "_times_", names(df)[j])
      new_df <- cbind(new_df, new_col)
      names(new_df)[ncol(new_df)] <- col_name
    }
  }

  return(new_df)
}

original_df <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C = c(7, 8, 9),
  D = c(10, 11, 12),
  E = c(13, 14, 15),
  F = c(16, 17, 18),
  G = c(19, 20, 21),
  H = c(22, 23, 24),
  I = c(25, 26, 27),
  J = c(28, 29, 30)
)

# Specify the columns you want to multiply (e.g., columns 1 and 3)
columns_to_multiply <- c(1, 3)

# Call the function with your data frame and specified columns
result_df <- multiply_columns_function(original_df, columns_to_multiply)

# Display the original and result data frames
print("Original Data Frame:")
print(original_df)

print("\nResult Data Frame:")
print(result_df)


# code segment #3
# this code multiplies each column by every other column, but only if
# the correlation is between two pre-specified thresholds

multiply_columns_by_correlation <- function(df, lower_threshold, upper_threshold) {
  num_columns <- ncol(df)
  new_df <- df  # Create a copy of the original data frame

  # Iterate through each combination of columns
  for (i in 1:num_columns) {
    for (j in 1:num_columns) {
      if (i != j) {  # Avoid multiplying a column by itself
        # Calculate correlation between columns i and j
        correlation <- cor(df[, i], df[, j])

        # Check if correlation is within the specified thresholds
        if (correlation >= lower_threshold && correlation <= upper_threshold) {
          # Multiply columns i and j and store the result in a new column
          new_col <- df[, i] * df[, j]
          col_name <- paste0(names(df)[i], "_times_", names(df)[j], "_corr_", round(correlation, 2))
          new_df <- cbind(new_df, new_col)
          names(new_df)[ncol(new_df)] <- col_name
        }
      }
    }
  }

  return(new_df)
}

# Example usage:
original_df <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C = c(7, 8, 9),
  D = c(10, 11, 12),
  E = c(13, 14, 15)
)

# Set correlation thresholds
lower_corr_threshold <- 0.05
upper_corr_threshold <- 0.99

# Call the function with your data frame and correlation thresholds
result_df <- multiply_columns_by_correlation(original_df, lower_corr_threshold, upper_corr_threshold)

# Display the original and result data frames
print("Original Data Frame:")
print(original_df)

print("\nResult Data Frame:")
print(result_df)




# Install and load the MASS package if not already installed
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)

# Function to generate a data frame with a specified correlation matrix
generate_correlated_df <- function(num_samples, correlation_matrix) {
  # Generate random samples from a multivariate normal distribution
  random_data <- mvrnorm(n = num_samples, mu = rep(0, ncol(correlation_matrix)), Sigma = correlation_matrix)

  # Convert the matrix to a data frame
  df <- as.data.frame(random_data)

  return(df)
}

# Example usage:
num_samples <- 100  # Adjust as needed
correlation_matrix <- matrix(c(1.0, 0.7, 0.3, 0.7, 1.0, 0.5, 0.3, 0.5, 1.0), nrow = 3, ncol = 3)

# Generate a data frame with the specified correlation matrix
correlated_df <- generate_correlated_df(num_samples, correlation_matrix)

# Display the correlation matrix of the generated data frame, as well as
# the generated data frame itself
print("Here is the data frame")
print(correlated_df)
print("Correlation Matrix of Generated Data Frame:")
print(cor(correlated_df))





