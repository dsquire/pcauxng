

# Define the function for integer coding of nominal data
integer_code <- function(data) {
  # Ensure the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  # Initialize an empty list to store the integer coded columns
  coded_list <- list()
  
  # Iterate over each column in the data frame
  for (col in names(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      # Convert character columns to factors
      if (is.character(data[[col]])) {
        data[[col]] <- factor(data[[col]])
      }
      
      # Convert factor levels to integers
      coded_list[[col]] <- as.integer(data[[col]])
    } else {
      # If the column is not a factor or character, keep it as is
      coded_list[[col]] <- data[[col]]
    }
  }
  
  # Combine all coded columns into a data frame
  coded_data <- as.data.frame(coded_list)
  
  return(coded_data)
}

# Example usage
# Create a sample data frame with nominal data
example_data <- data.frame(
  ID = 1:5,
  Gender = c("Male", "Female", "Female", "Male", "Female"),
  Occupation = c("Doctor", "Engineer", "Artist", "Doctor", "Artist")
)

# Apply the integer coding function
integer_coded_data <- integer_code(example_data)

# Print the result
print(integer_coded_data)

# To see the factor levels
print(levels(factor(example_data$Occupation)))
print(levels(factor(example_data$Gender)))