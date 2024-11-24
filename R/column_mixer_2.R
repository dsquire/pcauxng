
# This function was begun on 11/3
# as an overall upgrade on the function column_mixer()

source("convert_char_to_numeric_by_column.R")
source("get_flag.R")
source("find_correlation.R")
source("custom_multiply_columns.R")

column_mixer_2 <- function(df2, moderator_list){
  
  thresh <- 0.99
  nCols <- ncol(df2)
  
  # convert columns with char data to columns with
  # numeric data, and replace in the df
  df <- convert_char_to_numeric_by_column(df2)
  print("The converted df is:")
  print(df)
  for(element in moderator_list){
    for(i in 1:nCols){
      if (!(i %in% moderator_list)){
        
        # This will determine if the two columns correlate
        # higher than the threshold specified
        correl <- find_correlation(df[[element]], df[[i]])
        print("waypoint1 the df[[element]] is:")
        print(df[[element]])
        print("waypoint1 the df[[i]] is:")
        print(df[[i]])
        
        
        # This will check for a number of other scenarios where
        # the data should not be moved forward
        flag <- get_flag(df[[element]], df[[i]], correl, thresh)
      
        # Here, we actually multiply the columns
        # and append them to the data frame
        if(!flag){
          
          # This is the line that actually multiplies the two 
          # columns by one another.
          
          print("df[[element]] is:")
          print(df[[element]])
          print("df[[i]] is:")
          print(df[[i]])
          print("element is:")
          print(element)
          print("i is:")
          print(i)
          
          print(" ************** ")
          print(df[-1][[element]])
          print(df[-1][[i]])
          
          # d <- df[[element]]*df[[i]]
          moderator <- df[[element]]
          non_mod <- df[[i]]
          
          
          new_column <- custom_multiply_columns(moderator, non_mod)
          print("The new column is:")
          print(new_column)
          
          # These next two lines actually create the new column
          # and append it to the existing dataframe with the 
          # new column, and an appropriate name indicating
          # which two columns were multiplied.
          new_column_name <- paste("mix", element, i, sep = "_")
          df <- cbind(df, setNames(data.frame(new_column), new_column_name))
        } # END if(!flag)
      } # END if (!(i ...
    } # END for i ...
  } # END for(element ...
  return(df)
} # END function

generate_test_dataframe <- function(n_nominal, n_ordinal, n_continuous, n_rows) {
  # Initialize an empty list to store the columns
  columns <- list()
  
  # Generate nominal columns
  for (i in 1:n_nominal) {
    # Randomly assign nominal categories (e.g., 3 categories: "A", "B", "C")
    categories <- sample(LETTERS[1:3], n_rows - 1, replace = TRUE)
    col_name <- paste0("nominal_", i)
    columns[[col_name]] <- c("nominal", categories)  # First row is the label
  }
  
  # Generate ordinal columns
  for (i in 1:n_ordinal) {
    # Randomly assign ordinal values (e.g., 1, 2, 3 representing ordered categories)
    ordinals <- sample(1:5, n_rows - 1, replace = TRUE)
    col_name <- paste0("ordinal_", i)
    columns[[col_name]] <- c("ordinal", ordinals)  # First row is the label
  }
  
  # Generate continuous columns
  for (i in 1:n_continuous) {
    # Randomly assign continuous values (e.g., from a normal distribution)
    continuous <- round(rnorm(n_rows - 1, mean = 0, sd = 1), 2)
    col_name <- paste0("continuous_", i)
    columns[[col_name]] <- c("continuous", continuous)  # First row is the label
  }
  
  # Combine all columns into a dataframe
  df <- as.data.frame(columns)
  
  return(df)
}

options(error = NULL)

# Example usage
set.seed(128)  # For reproducibility
test_df <- generate_test_dataframe(n_nominal = 2, n_ordinal = 2, n_continuous = 2, n_rows = 10)
print(test_df)

mixed_df <- column_mixer_2(test_df, c(1, 2))
print(mixed_df)