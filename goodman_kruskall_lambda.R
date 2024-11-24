

goodman_kruskal_lambda <- function(x, y) {
  # Create a contingency table
  tbl <- table(x, y)
  
  # Calculate row and column sums
  row_totals <- rowSums(tbl)
  col_totals <- colSums(tbl)
  total <- sum(tbl)
  
  # Calculate maximum frequency in each row and overall
  max_row <- max(row_totals)
  max_col <- max(col_totals)
  
  # Lambda formula
  lambda <- (max_row - max(tbl)) / (total - max_col)
  return(lambda)
}
