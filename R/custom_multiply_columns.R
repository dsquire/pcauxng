

custom_multiply_columns <- function(column1, column2){
  
  output <- c(100)
  for(i in 2:length(column1)){
    
    cell <- as.numeric(column1[i]) * as.numeric(column2[i])
    output <- c(output, cell)
  }
  
  return(output)
}