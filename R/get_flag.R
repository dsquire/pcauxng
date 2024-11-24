

get_flag <- function(column_i, column_j, correl, thresh){
  
  
  # This will determine if the two columns correlate
  # higher than the threshold specified
  correl <- find_correlation(column_i, column_j)
  
  # Flag the columns for non-mixing if any of a variety
  # of conditions are true
  flag <- FALSE
  if(correl >= thresh){
    flag <- TRUE
  }
  if(any(is.na(column_i))){
    flag <- TRUE
  }
  if(any(is.na(column_j))){
    flag <- TRUE
  }
  if(length(column_i != length(column_j))){
    flag <- TRUE
  }
  if(length(column_i == 0)){
    flag <- TRUE
  }
  if(length(column_j == 0)){
    flag <- TRUE
  }
  return(FALSE)
}