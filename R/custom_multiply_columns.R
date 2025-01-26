# ******************************************************************************
#
# This function multiplies the two columns that it received as input and
# returns the product in the form of a column.
#
# ******************************************************************************

custom_multiply_columns <- function(column1, column2){
  
  output <- c()
  for(i in 1:length(column1)){
    
    cell <- as.numeric(column1[i]) * as.numeric(column2[i])
    
    # Concatenate the product from above to the vectors that
    # we're going to put out as output.
    output <- c(output, cell)
  }
  
  return(output)
}