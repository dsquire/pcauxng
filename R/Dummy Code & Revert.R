

# Function to perform dummy coding and revert to original form
process_data <- function(data, code_vector) {
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # Check if code_vector has the same length as the number of columns in the data
  if (length(code_vector) != ncol(data)) {
    stop("The length of code_vector must match the number of columns in the data frame.")
  }

  # Identify which columns to dummy code
  dummy_columns <- which(code_vector == 1)

  # Dummy code specified columns
  dummy_coded_data <- data
  dummy_names <- colnames(data)[dummy_columns]

  for (col_idx in dummy_columns) {
    column_name <- colnames(data)[col_idx]
    column_data <- data[[column_name]]

    # Get unique levels
    levels <- unique(column_data)

    # Create dummy coded columns
    dummy_coded <- as.data.frame(matrix(0, nrow = nrow(data), ncol = length(levels)))
    colnames(dummy_coded) <- paste0(column_name, "_", levels)

    for (i in seq_along(levels)) {
      dummy_coded[[i]] <- as.integer(column_data == levels[i])
    }

    # Bind dummy coded columns to the original data
    dummy_coded_data <- cbind(dummy_coded_data, dummy_coded)

    # Remove the original column
    dummy_coded_data <- dummy_coded_data[, -col_idx]
  }

  # Print dummy coded data
  cat("Dummy Coded Data:\n")
  print(dummy_coded_data)

  # Function to revert dummy coded data to original form
  revert_dummy_coding <- function(dummy_data, dummy_names) {
    # Identify dummy columns
    dummy_columns_names <- colnames(dummy_data)[sapply(dummy_data, function(x) all(x %in% c(0, 1)))]

    # Revert dummy coded data to original categorical data
    if (length(dummy_columns_names) == 0) {
      stop("No dummy coded columns found in the data frame.")
    }

    # Get the levels (names of dummy columns)
    levels <- gsub("^[^_]+_", "", dummy_columns_names)

    original_categorical <- apply(dummy_data, 1, function(row) {
      # Find the index of the column with value 1
      idx <- which(row == 1)
      if (length(idx) == 1) {
        return(levels[idx])
      } else {
        return(NA) # Return NA if the row doesn't correspond to a single level
      }
    })
    print(original_categorical)
    # Return as a factor
    return(factor(original_categorical, levels = unique(levels)))
  }

  # Extract dummy columns from dummy coded data
  dummy_columns_data <- dummy_coded_data[, sapply(dummy_coded_data, function(x) all(x %in% c(0, 1)))]

  # Revert dummy coded data to original categorical form
  reverted_data <- revert_dummy_coding(dummy_columns_data, dummy_names)

  # Print reverted data
  cat("\nReverted Data:\n")
  print(reverted_data)
}

# Example usage
original_data <- data.frame(
  id = 1:5,
  color = c("red", "blue", "green", "red", "green"),
  size = c("small", "large", "medium", "large", "small"),
  weight = c(10, 20, 30, 40, 50)
)

code_vector <- c(0, 1, 1, 0) # Dummy code both columns

process_data(original_data, code_vector)
