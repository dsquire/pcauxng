

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



# *****************************************************************************
# **********    All code above this line is for data preparation     **********
# *****************************************************************************

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


# Example usage:
num_samples <- 100  # Adjust as needed
correlation_matrix <- matrix(c(1.0, 0.7, 0.3, 0.7, 1.0, 0.5, 0.3, 0.5, 1.0), nrow = 3, ncol = 3)

# Generate a data frame with the specified correlation matrix
correlated_df <- generate_correlated_df(num_samples, correlation_matrix)


# ******************************************************************************
# *******  Everything below this line is just to display the output    *********
# ******************************************************************************

# Display the correlation matrix of the generated data frame, as well as
# the generated data frame itself
print("Here is the data frame")
print(correlated_df)
print("Correlation Matrix of Generated Data Frame:")
print(cor(correlated_df))

# Call the function with your data frame
result_df <- multiply_columns(original_df)

# Display the original and result data frames
print("Original Data Frame:")
print(original_df)

print("\nResult Data Frame:")
print(result_df)
