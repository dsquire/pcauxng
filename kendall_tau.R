

kendall_tau <- function(x, y) {
  if(is.character(x)){
    x <- convert_char_factor_to_numeric(x)
  }
  if(is.character(y)){
    y <- convert_char_factor_to_numeric(y)
  }
  n <- length(x)
  concordant <- 0
  discordant <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      
      concordant <- concordant + sign(x[i] - x[j]) * sign(y[i] - y[j])
    }
  }
  tau <- concordant / (n * (n - 1) / 2)
  return(tau)
}
