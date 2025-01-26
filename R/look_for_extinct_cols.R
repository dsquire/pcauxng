
look_for_extinct_cols <- function(a, b, extinction_vec){
  
  
  tempA <- as.double(unlist(extinction_vec[a]))
  tempB <- as.double(unlist(extinction_vec[b]))
  
  if(tempA == 1){
    return(TRUE)
  }
  if(tempB == 1){
    return(TRUE)
  } else {return(FALSE)}
}


