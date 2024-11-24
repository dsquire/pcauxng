# **********************************************************
#
#   This set of functions takes nominal data and dummy codes 
#   it.  The output has the least frequently occurring level 
#   hidden.
#
# **********************************************************


find_least_frequent_level <- function(data, nominal_var) {
  # Ensure nominal_var is a factor
  if (!is.factor(data[[nominal_var]])) {
    stop("The variable must be a factor.")
  }
  
  # Calculate the frequency of each level
  freq_table <- table(data[[nominal_var]])
  
  # Find the level with the minimum frequency
  least_frequent_level <- names(freq_table)[which.min(freq_table)]
  
  return(least_frequent_level)
}

dummy_code_nominal_hide_least <- function(data, nominal_var) {
  # Ensure nominal_var is a factor
  if (!is.factor(data[[nominal_var]])) {
    stop("The variable must be a factor.")
  }
  
  # Find the least frequent level
  hide_level <- find_least_frequent_level(data, nominal_var)
  
  # Get the levels of the nominal variable
  levels <- levels(data[[nominal_var]])
  
  # Create dummy variables for each level except the hidden one
  for (level in levels) {
    if (level != hide_level) {
      dummy_var <- ifelse(data[[nominal_var]] == level, 1, 0)
      data[[paste0(nominal_var, "_", level)]] <- dummy_var
    }
  }
  
  # Remove the original factor variable
  data[[nominal_var]] <- NULL
  
  return(data)
}

# Example usage
# Create a sample data frame
df <- data.frame(
  id = 1:5,
  color = factor(c('red', 'blue', 'green', 'blue', 'red'))
)

# Apply the dummy coding function with hiding the least frequent level
dummy_coded_df <- dummy_code_nominal_hide_least(df, 'color')
print(dummy_coded_df)
