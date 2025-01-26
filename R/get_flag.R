# ******************************************************************************
#
# This function returns 'TRUE' if any of the following conditions are met
# regarding 'column_i' and 'column_j':
# 1.) they correlate greater than 'thresh'
# 2.) either of the columns have NA values
# 3.) the columns are of dissimilar length or of length zero. BC 1/26/2025
#
# ******************************************************************************

get_flag <- function(column_i, column_j, correl, thresh){
  
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
  return(flag)
}