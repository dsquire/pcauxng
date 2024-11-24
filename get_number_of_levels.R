

get_number_of_levels <- function(x, y){
  levelsX <- length(unique(x))
  levelsY <- length(unique(y))
  return(levelsX + levelsY)
  
}