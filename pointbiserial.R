

pointbiserial <- function(x, y) {
  # Ensure y has exactly two unique values
  if (length(unique(y)) != 2) {
    stop("y must be a binary variable with exactly two unique values.")
  }
  
  # Convert y to a binary numeric variable (0 and 1)
  y_numeric <- (as.factor(y)) - 1  # Factor levels converted to 1, 2, so subtract 1
  
  # Calculate means for group 1 (y = 1) and group 0 (y = 0)
  mean_x1 <- mean(x[y_numeric == 1])
  mean_x0 <- mean(x[y_numeric == 0])
  
  # Standard deviation of the continuous variable
  s_x <- sd(x)
  
  # Number of cases in each group
  n1 <- sum(y_numeric == 1)
  n0 <- sum(y_numeric == 0)
  n <- length(y_numeric)
  
  # Point-biserial correlation formula
  r_pb <- (mean_x1 - mean_x0) / s_x * sqrt((n1 * n0) / n^2)
  
  return(r_pb)
}