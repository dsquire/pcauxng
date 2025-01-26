drop_correlated_columns <- function(data, corr_matrix, threshold) {
  # Get the upper triangular part of the correlation matrix
  upper_tri <- upper.tri(corr_matrix, diag = TRUE)
  
  # Find pairs of columns with correlation higher than the threshold
  correlated_pairs <- which(abs(corr_matrix) > threshold & upper_tri, arr.ind = TRUE)
  
  # Find columns with more NAs in case of high correlation
  columns_to_drop <- vector("list", length = nrow(correlated_pairs))
  
  for (i in seq_len(nrow(correlated_pairs))) {
    col1 <- correlated_pairs[i, 1]
    col2 <- correlated_pairs[i, 2]
    
    if (sum(is.na(data[, col1])) > sum(is.na(data[, col2]))) {
      columns_to_drop[[i]] <- col1
    } else {
      columns_to_drop[[i]] <- col2
    }
  }
  
  # Drop the identified columns from the data frame
  data_clean <- data[, -unlist(columns_to_drop)]
  
  # Return the cleaned data frame
  return(data_clean)
}



# ******************************************************************************
# ******************************************************************************
# *********     Code below this line is strictly for testing          **********
# *********     the function.                                         **********
# ******************************************************************************
# ******************************************************************************


# Generate sample data
set.seed(123)
data <- data.frame(
  A = rnorm(100),
  B = rnorm(100),
  C = rnorm(100),
  D = rnorm(100)
)

data

# Introduce some correlation between columns B and C
data$C <- data$B + rnorm(100, mean = 0, sd = 0.2)

# Compute correlation matrix
cor_matrix <- cor(data)

# Set threshold for correlation
threshold <- 0.7

# Test the function
cleaned_data <- drop_correlated_columns(data, cor_matrix, threshold)

# Check if correlated columns are dropped
print(cleaned_data)