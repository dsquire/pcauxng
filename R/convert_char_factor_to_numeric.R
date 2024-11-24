
convert_char_factor_to_numeric <- function(factor_data) {
  # Ensure the input is a factor
  factor_data <- as.factor(factor_data)
  
  # Convert the factor to its underlying numeric representation (levels)
  numeric_data <- as.numeric(factor_data)
  
  return(numeric_data)
}