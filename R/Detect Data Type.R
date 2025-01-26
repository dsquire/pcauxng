is_ordinal <- function(vector) {

  vector_of_unique_elements <- unique(vector)
  num_unique <- length(vector_of_unique_elements)

  if(num_unique < 5){
    boolean <- TRUE
  }
  else(
    boolean <- FALSE
  )
  return(boolean)
}


detect_data_types <- function(data) {
  result <- list()

  for (col in names(data)) {
    column <- data[[col]]

    # Check if the column contains only characters (factors)
    if (is.factor(column) || is.character(column)) {
      result[[col]] <- "Nominal"
      next
    }

    # Check if the column contains only integers or factors with integer levels
    booleanOrdinal <- is_ordinal(column)
    if(booleanOrdinal == TRUE){
      result[[col]] <- "Ordinal"
      print("flag for Ordinal data")
      next
    }

    # Check to see if any of the columns are decimal values
    if(is.numeric(column) || !is.integer(column)){
      result[[col]] <- "Continuous"
      next
    }


    # If none of the above conditions are met, classify as "Unknown"
    result[[col]] <- "Unknown"
  }

  return(result)
}

# *****************************************************************************
# *****************************************************************************
# **********       Everything below this line is just             *************
# **********       prototyping material for testing               *************
# **********      the function above.                             *************
# *****************************************************************************
# *****************************************************************************


# Example usage:
# Create some sample data
data <- data.frame(
  A = c("Male", "Female", "Male", "Male", "Male", "Female", "Male"),
  B = c(1, 2, 3, 4, 5, 6, 7),
  C = c(10, 20, 30, 20, 10, 20, 30),
  D = c(1.5, 2.7, 3.9, 4.5, 5.5, 7.8, 9.9)
)

# Determine the data types of each column
types <- detect_data_types(data)

# Print the result
print(types)
