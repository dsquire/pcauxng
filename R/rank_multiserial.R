
source("check_for_three_levels.R")
rank_multiserial <- function(x, y){
  
  # Check which variable is categorical (factor/character)
  if (is.character(x)) {
    # If 'x' is categorical and 'y' is continuous, swap them
    temp <- x
    x <- y
    y <- temp
  }
  
  if(is.character(y)){
    y <- convert_char_factor_to_numeric(y)
  }
  
  has_three_levels <- check_for_three_levels(x, y)
  if(!has_three_levels){
    stop("Error by Brent: it was determined during a rank_multiserial 
         correlation that one of the data columns did not have three levels")
  }
  
  # Rank the numeric variable
  ranked_numeric <- rank(x)
  ranked_categoric <- rank(y)
  
  # Create a data frame with the ranks and categorical values
  data <- data.frame(A = ranked_numeric, B = ranked_categoric)
  
  n <- length(data$B)
  d_squared_accum <- 0
  for(i in 1:n){
    d_squared <- (data$A[i] - data$B[i])^2
    d_squared_accum <- d_squared_accum + d_squared
  }
  
  formula <- 1 - (6*d_squared_accum/(n*((n^2)-1)))
  
  return(formula)
}