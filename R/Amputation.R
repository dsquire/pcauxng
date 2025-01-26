# Function to randomly delete N data points from specified columns in a data frame
delete_random_values <- function(df, columns_to_delete, N) {
  result_df <- df

  # Iterate through specified columns
  for (col in columns_to_delete) {
    # Check if the column exists in the data frame
    if (col %in% names(df)) {
      # Identify N random indices to delete
      indices_to_delete <- sample(seq_along(result_df[[col]]), N)

      # Replace specified indices with NA
      result_df[[col]][indices_to_delete] <- NA
    }
  }

  return(result_df)
}

# Example usage:
my_df <- data.frame(
  A = c(1, 2, 3, 4, 5),
  B = c("apple", "banana", "orange", "apple", "grape"),
  C = c(TRUE, FALSE, TRUE, TRUE, FALSE)
)

# Specify columns and number of data points to delete
columns_to_delete <- c("A", "B", "C")
N <- 2

# Delete N random data points from specified columns in the data frame
result_df <- delete_random_values(my_df, columns_to_delete, N)

# Display the original and result data frames
print("Original Data Frame:")
print(my_df)

print("\nResult Data Frame:")
print(result_df)
