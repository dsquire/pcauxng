# **********************************************************
#
#   This set of functions takes nominal data and dummy codes
#   it. In this particular case, the user specifies the
#   which level is the basis level.
#
# **********************************************************



dummy_code_nominal <- function(data, nominal_var, hide_level = NULL) {
  # Ensure nominal_var is a factor
  if (!is.factor(data[[nominal_var]])) {
    stop("The variable must be a factor.")
  }

  # Get the levels of the nominal variable
  levels <- levels(data[[nominal_var]])

  # Check if the hide_level is valid
  if (!is.null(hide_level) && !(hide_level %in% levels)) {
    stop("The specified hide_level is not a valid level of the nominal variable.")
  }

  # Create dummy variables for each level except the hidden one
  for (level in levels) {
    if (is.null(hide_level) || level != hide_level) {
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
#df <- data.frame(
 # id = 1:5,
  #color = factor(c('red', 'blue', 'green', 'blue', 'red'))
#)



df <- read.csv("C:\Users\brent\Desktop\SDT, SC, & IGD - For Sharing.csv",
         header = TRUE,
         sep = ",",
         na.strings = "NA")



# Apply the dummy coding function with hiding a level
dummy_coded_df <- dummy_code_nominal(df)
print(dummy_coded_df)


