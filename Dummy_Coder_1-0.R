# This function takes nominal data and dummy codes it.

# It will take data like:
#
# col0 <- 1:4
# col1 <- c("Male", "Female", "Female", "Male")
# col2 <- c("Lawyer", "Doctor", "Artist", "Doctor")
#
# . . . and produce output in the form of:
# ID   Male     Lawyer     Doctor
# 1    1        1           0
# 2    0        0           1
# 3    0        0           0
# 4    1        0           1


# Define the function for dummy coding
dummy_code <- function(data) {
  # Ensure the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Initialize an empty list to store the dummy coded columns
  dummy_list <- list()

  # Iterate over each column in the data frame
  for (col in names(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      # Convert character columns to factors
      if (is.character(data[[col]])) {
        data[[col]] <- factor(data[[col]])
      }

      # Get the levels of the factor
      levels <- levels(data[[col]])

      # Create dummy variables for each level except the first one
      for (level in levels[-1]) {
        dummy_name <- paste(col, level, sep = "_")
        dummy_list[[dummy_name]] <- as.numeric(data[[col]] == level)
      }
    } else {
      # If the column is not a factor or character, keep it as is
      dummy_list[[col]] <- data[[col]]
    }
  }

  # Combine all dummy coded columns into a data frame
  dummy_data <- as.data.frame(dummy_list)

  return(dummy_data)
}

# Example usage
# Create a sample data frame with nominal data
example_data <- data.frame(

  ID = 1:7,
  Gender = c("Male", "Female", "Female", "Male", "Female", "Female", "Female"),
  Occupation = c("Doctor", "Engineer", "Artist", "Doctor", "Artist", "Lawyer", "Lawyer")

)

example_data <- read.csv("C:/Users/brent/Desktop/SDT, SC, & IGD - For Sharing.csv",
               header = TRUE,
               sep = ",",
               na.strings = "NA")



# Apply the dummy coding function
dummy_coded_data <- dummy_code(example_data)

# Print the result
print(dummy_coded_data)
