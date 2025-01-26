

update_extinction_vec <- function(i, j, column_i, column_j, extinction_vec, missing_vec, correl, thresh){
  
  if(correl >= thresh){
    if(missing_vec[i] >= missing_vec[j]){
      extinction_vec[i] <- 1
    } else {
      extinction_vec[j] <- 1
    }
  } 
  
  return(extinction_vec)
}