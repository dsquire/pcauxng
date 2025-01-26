
install.packages("vcd")
library(vcd)


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


data_cor <- function(vector1, vector2){
  type_1 <- detect_data_types(vector1)
  type_2 <- detect_data_types(vector2)
  if(type_1 == "Nominal"){
    if(type_2 == "Nominal"){
      
      # Create a contingency table (replace with your own data)
      my_table <- table(vector1, vector2)
      
      # Compute Goodman and Kruskal's lambda
      correl <- assocstats(my_table)$lambda
      
      
    }
    if(type_2 == "Ordinal"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "biserial")
      
    }
    if(type_2 == "Continuous"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "biserial")
      
    }
  }
  if(type_1 == "Ordinal"){
    if(type_2 == "Nominal"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "biserial")
      
    }
    if(type_2 == "Ordinal"){
      
      # Put Kendall's Tau(b) here
      correl <- correlation(data = vector1, data2 = vector2, method = "kendall")
      
    }
    if(type_2 == "Continuous"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "kendall")
      
    }
  }
  if(type_1 == "Continuous"){
    if(type_2 == "Nominal"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "biserial")
      
    }
    if(type_2 == "Ordinal"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "kendall")
      
    }
    if(type_2 == "Continuous"){
      
      correl <- correlation(data = vector1, data2 = vector2, method = "pearson")
      
    }
  }
  return(correl)
  
}






