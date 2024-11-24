

check_for_three_levels <- function(x, y){
  levelsX <- length(unique(x))
  levelsY <- length(unique(y))
  
  has_three_levels <- FALSE
  if(levelsX >= 3 || levelsY >= 3){
    has_three_levels <- TRUE
  }
  return(has_three_levels)
}
