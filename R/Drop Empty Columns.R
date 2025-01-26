drop_empty_columns <- function(data) {
  # Find empty columns
  empty_columns <- sapply(data, function(x) all(is.na(x)))
  
  # Drop empty columns
  data_clean <- data[, !empty_columns, drop = FALSE]
  
  # Get names of dropped columns
  dropped_columns <- names(data)[empty_columns]
  
  return(list(clean_data = data_clean, dropped_columns = dropped_columns))
}


# Example usage
data <- data.frame(
  A = c(1, NA, 3),
  B = c(NA, NA, NA),
  C = c(4, 5, NA)
)

result <- drop_empty_columns(data)

clean_data <- result$clean_data
dropped_columns <- result$dropped_columns

clean_data

dropped_columns