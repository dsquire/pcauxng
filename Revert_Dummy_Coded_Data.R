# Function to check if data is dummy coded
is_dummy_coded <- function(data) {
  # Check if all columns are binary (0 or 1)
  if (!all(sapply(data, function(x) all(x %in% c(0, 1))))) {
    return(FALSE)
  }
  
  # Check if each row has exactly one `1` and the rest are `0`s
  rows_valid <- apply(data, 1, function(row) sum(row) == 1)
  
  return(all(rows_valid))
}


# Function to revert dummy coded data to original categorical format
revert_dummy_coding <- function(dummy_data) {
  # Check if input is a data frame
  if (!is.data.frame(dummy_data)) {
    stop("Input must be a data frame")
  }
  
  # Check if the data is dummy coded
  if (!is_dummy_coded(dummy_data)) {
    stop("Data does not appear to be dummy coded")
  }
  
  # Identify dummy columns (assumes columns with all zeros or all ones are dummy coded)
  dummy_columns <- sapply(dummy_data, function(x) all(x %in% c(0, 1)))
  
  # Extract dummy columns
  dummies <- dummy_data[, dummy_columns, drop = FALSE]
  
  # Revert dummy coded data to original categorical data
  if (ncol(dummies) == 0) {
    stop("No dummy coded columns found in the data frame")
  }
  
  # Get the levels (names of dummy columns)
  levels <- colnames(dummies)
  
  # Reconstruct the original categorical variable
  original_categorical <- apply(dummies, 1, function(row) {
    # Find the index of the column with value 1 (assuming only one 1 per row)
    idx <- which(row == 1)
    if (length(idx) == 1) {
      return(levels[idx])
    } else {
      return(NA) # Return NA if the row doesn't correspond to a single level
    }
  })
  
  # Return as a factor
  return(factor(original_categorical, levels = levels))
}

# Example usage
dummy_data <- data.frame(
  cat_A = c(1, 0, 0, 1, 0),
  cat_B = c(0, 1, 0, 0, 1),
  cat_C = c(0, 0, 1, 0, 0)
)

reverted_data <- revert_dummy_coding(dummy_data)
print(reverted_data)