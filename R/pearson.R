


pearson <- function(x, y){
  if(is.character(x)){
    x <- convert_char_factor_to_numeric(x)
  }
  if(is.character(y)){
    y <- convert_char_factor_to_numeric(y)
  }
  correlation_to_return <- cor(x, y)
  return(correlation_to_return)
}