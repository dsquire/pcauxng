

# This is a function to calculate correlations between columns
# of a dataframe.  This function supports different types of
# correlations between different types of data, such as calculating
# the correlation between integer data and categorical data
# using the appropriate type of correlation.
# 
# Correlations are as outlined in:
# Khamis, H. (2008) JOURNAL OF DIAGNOSTIC MEDICAL SONOGRAPHY. VOL. 24, NO. 3
# with some adjustments continuous-nominal data pairs, which will be done
# using rank_biserial instead of point_biserial.

# To do:
#
# 1.)
# Write code that will skip lines of code in the df that have missing 
# values
#


compute_correlation_matrix <- function(df) {
  # Extract the first row, which contains the data types
  types <- df[1,]
  
  # Remove the first row to keep only the data
  df <- df[-1,]
  print(df)
  # Initialize an empty matrix to store correlations
  n <- ncol(df)
  correlation_matrix <- matrix(-999, n, n, dimnames = list(names(df), names(df)))
  
  # Loop over all pairs of columns
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      
      type1 <- types[i]
      type2 <- types[j]
      
      # Define correlation type based on the pair of data types
      if (type1 == "continuous" & type2 == "continuous") {
        correlation_type <- "pearson"
        
      } else if ((type1 == "continuous" & type2 == "ordinal") || (type1 == "ordinal" & type2 == "continuous")) {
        correlation_type <- "kendall"
        
      } else if ((type1 == "continuous" & type2 == "nominal") || (type1 == "nominal" & type2 == "continuous")) {
        correlation_type <- "rank_multiserial"
        
      } else if (type1 == "ordinal" & type2 == "ordinal") {
        correlation_type <- "kendall"
        
      } else if ((type1 == "ordinal" & type2 == "nominal") || (type1 == "nominal" & type2 == "ordinal")) {
        correlation_type <- "rank_multiserial"
        
      } else if (type1 == "nominal" & type2 == "nominal") {
        correlation_type <- "goodman_kruskal_lambda"
      }
      
      # Compute the appropriate correlation
      if (correlation_type == "pearson") {
        correlation_matrix[i, j] <- pearson(df[[i]], df[[j]])
        
      } else if (correlation_type == "kendall") {
        correlation_matrix[i, j] <- kendall_tau(df[[i]], df[[j]])
        
      } else if (correlation_type == "rank_multiserial") {
        correlation_matrix[i, j] <- rank_multiserial(df[[i]], df[[j]])
        
      } else if (correlation_type == "rank_multiserial") {
        correlation_matrix[i, j] <- rank_multiserial(df[[i]], df[[j]])
        
      } else if (correlation_type == "goodman_kruskal_lambda") {
        correlation_matrix[i, j] <- goodman_kruskal_lambda(df[[i]], df[[j]])
        
      }
      
      # Symmetry in correlation matrix
      correlation_matrix[j, i] <- correlation_matrix[i, j]
    }
  }
  
 
  
  return(correlation_matrix)
}

get_data_types <- function(df){
  
  types <- df[1,]
  
  return(types)
}

remove_data_types <- function(df){
  
  # Remove the first row to keep only the data
  df <- df[-1,]
  print(df)
  
  return(df)
}

convert_char_factor_to_numeric <- function(factor_data) {
  # Ensure the input is a factor
  factor_data <- as.factor(factor_data)
  
  # Convert the factor to its underlying numeric representation (levels)
  numeric_data <- as.numeric(factor_data)
  
  return(numeric_data)
}

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

# Kendall's Tau (Coefficient of Rank Correlation) without libraries
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

# Point-biserial function without external dependencies
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
  
  # Insert code here to make sure the y variable has at least three levels.
  #
  #
  
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

# Goodman and Kruskal's Lambda
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


# **************************************************************************
# **************************************************************************
#
# ***           Code below this line is strictly for generating
#               testing data.
#
# **************************************************************************
# **************************************************************************


generate_test_dataframe <- function(n_nominal, n_ordinal, n_continuous, n_rows) {
  # Initialize an empty list to store the columns
  columns <- list()
  
  # Generate nominal columns
  for (i in 1:n_nominal) {
    # Randomly assign nominal categories (e.g., 3 categories: "A", "B", "C")
    categories <- sample(LETTERS[1:3], n_rows - 1, replace = TRUE)
    col_name <- paste0("nominal_", i)
    columns[[col_name]] <- c("nominal", categories)  # First row is the label
  }
  
  # Generate ordinal columns
  for (i in 1:n_ordinal) {
    # Randomly assign ordinal values (e.g., 1, 2, 3 representing ordered categories)
    ordinals <- sample(1:5, n_rows - 1, replace = TRUE)
    col_name <- paste0("ordinal_", i)
    columns[[col_name]] <- c("ordinal", ordinals)  # First row is the label
  }
  
  # Generate continuous columns
  for (i in 1:n_continuous) {
    # Randomly assign continuous values (e.g., from a normal distribution)
    continuous <- round(rnorm(n_rows - 1, mean = 0, sd = 1), 2)
    col_name <- paste0("continuous_", i)
    columns[[col_name]] <- c("continuous", continuous)  # First row is the label
  }
  
  # Combine all columns into a dataframe
  df <- as.data.frame(columns)
  
  return(df)
}

# Example usage
set.seed(128)  # For reproducibility
test_df <- generate_test_dataframe(n_nominal = 2, n_ordinal = 2, n_continuous = 2, n_rows = 10)
print(test_df)

corr_matrix <- compute_correlation_matrix(test_df)
print(corr_matrix)

