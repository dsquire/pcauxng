

goodman_kruskal_lambda <- function(x, y) {
  # Create a contingency table
  tbl <- table(x, y)
  print(tbl)
 
  
  # Calculate column sums
  col_totals <- colSums(tbl)
  row_totals <- rowSums(tbl)
  tbl_total <- sum(col_totals)
  
  T3 <- max(col_totals)
  T4 <- max(row_totals)
  
  
  row_maxima <- c()
  for(i in 1:ncol(tbl)){
    
    row <- tbl[,i]
    min_val <- max(row)
    row_maxima <- c(row_maxima, min_val)
  }
  
  col_maxima <- c()
  for(i in 1:nrow(tbl)){
    column <- tbl[i,]
    min_val <- max(column)
    col_maxima <- c(col_maxima, min_val)
  }
  
  T1 <- sum(row_maxima)
  T2 <- sum(col_maxima)
  
  lambda <- (T1 + T2 - T3 - T4)/(2*tbl_total - T3 - T4)
  
  return(lambda)
  
}

i_v = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C")
d_v = c("X", "X", "Y", "Y", "Y", "Z", "X", "Z", "Z", "Z")

vec_x <- c(0, 1, 0, 1, 2, 2, 2, 1, 1)
vec_y <- c(0, 1, 0, 1, 2, 2, 2, 1, 1)


# Generating example data for Goodman and Kruskal's lambda correlation
set.seed(123)  # For reproducibility

# Generate 200 values for the independent variable with 3 categories: A, B, C
iv <- sample(c("A", "B", "C"), size = 200, replace = TRUE, prob = c(0.4, 0.3, 0.3))

# Generate 200 values for the dependent variable with 3 categories: X, Y, Z
dv <- sample(c("X", "Y", "Z"), size = 200, replace = TRUE, prob = c(0.5, 0.3, 0.2))

# Combine into a data frame
data <- data.frame(i_v, d_v)

output <- goodman_kruskal_lambda(i_v, d_v)
print(output)

library("DescTools")
output2 <- Lambda(i_v, d_v)
print(output2)

