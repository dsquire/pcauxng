
#' @description
#' Dummy codes columns in a data frame, columns are specified by the user
#' with support for ranges
#' @return a data.frame with the specified columns dummy coded
dummy_code_columns <- function(df, cols_to_dummy) {
  
  # Convert range specifications into individual column names
  cols_expanded <- unlist(lapply(cols_to_dummy, function(col) {
    if (grepl("-", col)) {
      # Extract column range
      parts <- strsplit(col, " - ")[[1]]
      start_col <- parts[1]
      end_col <- parts[2]
      
      # Get indices of the start and end columns
      start_index <- match(start_col, names(df))
      end_index <- match(end_col, names(df))
      
      # Generate a vector of column names in the range
      return(names(df)[start_index:end_index])
    } else {
      return(col)
    }
  }))
  
  # Replace original columns with dummy variables
  for (col in cols_expanded) {
    if (col %in% names(df)) {
      
      # Convert the specified column to a factor
      df[[col]] <- as.factor(df[[col]])
      
      # Create dummy variables for each unique value in the column
      dummies <- model.matrix(~ df[[col]] - 1)
      
      # Rename columns to avoid clashes
      colnames(dummies) <- paste0(col, "_", colnames(dummies))
      
      # Remove the original column from the data frame
      df[[col]] <- NULL
      
      # Bind the dummy columns to the original data frame
      df <- cbind(df, dummies)
    } else {
      warning(paste("Column", col, "not found in the data frame."))
    }
  }
  
  return(df)
}

