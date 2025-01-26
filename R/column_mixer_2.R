
# This function was begun on 11/3
# ******************************************************************************
# 
#   This function takes as input a data frame along with other metadata and 
#   returns a data frame where:
#   1.) The columns have been multiplied by one another, and the resulting
#       product columns have been given meaningful names.
#   2.) Any columns consisting of characters rather than numbers have been
#       converted to a numeric data type.
#   3.) The column multiplication has been carried out according to 
#       moderator x non-moderator logic.  Moderators are not multiplied by 
#       other moderators, and non-moderators are not multiplied by non-
#       moderators.
#   3.) Any columns pairs whose correlation exceeds the value of "thresh"
#       have not been multiplied.
#   4.) Correlations calculated in (3) above have been done appropriately
#       by data type, according to the paper
#
#   NOTE: As of 1/26/2025, there is a print statement (around line 51 or so)
#         that is for prototyping, and should be removed.
# 
# ******************************************************************************

source("convert_char_to_numeric_by_column.R")
source("get_flag.R")
source("find_correlation.R")
source("custom_multiply_columns.R")
source("determine_data_type.R")
source("look_for_extinct_cols.R")
source("prepare_extinction_vec.R")
source("update_extinction_vec.R")
source("advanced_match.R")

column_mixer_2 <- function(df2, moderator_list, nominalvars, ordinalvars, continousvars, missing_vec){
  
  # This is the threshold that, if exceeded, triggers the decision NOT to multiply
  # two columns together.
  thresh <- 0.90
  
  # In order to iterate over the data frame input, we need to know the number of columns it has.
  nCols <- ncol(df2)
  
  # The extinction vector is a vector of column numbers that have been determined to correlate
  # (greater than the "thresh" value) with some other column in the data frame.  Here,
  # we are simply creating the extinction_vec.
  extinction_vec <- prepare_extinction_vec(df2)
  
  # convert columns with char data to columns with
  # numeric data, and replace in the df
  df <- convert_char_to_numeric_by_column(df2)
  
  # This print statement is for testing purposes, and will not be included
  # in the final PcAuxNG.
  print("The converted df is:")
  print(df)
  
  # This is the for-next loop that iterates over the data frame (and moderator_list),
  # determining whether each column should be multiplied or not (per their correlation)
  # with non-moderator columns.
  for(element in moderator_list){
    for(i in 1:nCols){
      
      # This if-statement only executes if column "i" is not listed in "moderator_list".
      if (!(i %in% moderator_list)){
        
        data_type1 <- determine_data_type(element, nominalvars, ordinalvars, continuousvars)
        data_type2 <- determine_data_type(i, nominalvars, ordinalvars, continuousvars)
        
     
        correl <- find_correlation(df[[element]], df[[i]], data_type1, data_type2)
        
        # Flag1 will be set to "TRUE" if the two columns "df[[element]] and df[[i]]
        # correlate greater that "thresh".
        flag1 <- get_flag(df[[element]], df[[i]], correl, thresh)
       
        
        # This if-statement updates the extinction vector so that the column with more missing values
        # 'goes extinct' (i.e. won't be multiplied with any other columns in future multiplications).
        # The extinction vector falls out of scope once this instance of column_mixer goes out of scope.
        if(flag1){
          extinction_vec <- update_extinction_vec(element, i, df[[element]], df[[i]], extinction_vec, missing_vec, correl, thresh)
        }
      
        # This flag will be set to TRUE if either the current 'element' or 'i' columns
        # have been listed in the extinction vector.
        flag2 <- look_for_extinct_cols(element, i, extinction_vec)
        
        # Here, we actually multiply the columns
        # and append them to the data frame
        if((!flag1) && (!flag2)){
            # This is the line that multiplies the two 
            # columns by one another.
            moderator <- df[[element]]
            non_mod <- df[[i]]
            new_column <- custom_multiply_columns(moderator, non_mod)
            
            # These next two lines actually create the new column
            # and append it to the existing dataframe with the 
            # new column, and an appropriate name indicating
            # which two columns were multiplied.
            new_column_name <- paste("aux", element, i, sep = "_")
            df <- cbind(df, setNames(data.frame(new_column), new_column_name))
            
        } # END if(!flag)
      } # END if (!(i ...
    } # END for i ...
  } # END for(element ...
  return(df)
} # END function

# *********************************************************************************
#
#     Everything below this line is for testing purposes and will be removed
#     from the final version of PcAuxNG. (BC 1/26/2025)
#
# *********************************************************************************

generate_test_dataframe <- function(n_nominal, n_ordinal, n_continuous, n_rows) {
  # Initialize an empty list to store the columns
  columns <- list()
  
  # Generate nominal columns
  categories <- sample(LETTERS[1:3], n_rows, replace = TRUE)
  for (i in 1:n_nominal) {
    # Randomly assign nominal categories (e.g., 3 categories: "A", "B", "C")
    
    col_name <- paste0("nominal_", i)
    columns[[col_name]] <- c(categories)  
  }
  
  categories <- columns[[col_name]]
  col_name <- paste0("nominal_", i+1)
  columns[[col_name]] <- c(categories)
  
  # Generate ordinal columns
  for (i in 1:n_ordinal) {
    # Randomly assign ordinal values (e.g., 1, 2, 3 representing ordered categories)
    ordinals <- sample(1:5, n_rows, replace = TRUE)
    col_name <- paste0("ordinal_", i)
    columns[[col_name]] <- c(ordinals)  
  }
  
  # Generate continuous columns
  for (i in 1:n_continuous) {
    # Randomly assign continuous values (e.g., from a normal distribution)
    continuous <- round(rnorm(n_rows, mean = 0, sd = 1), 2)
    col_name <- paste0("continuous_", i)
    columns[[col_name]] <- c(continuous)  
  }
  
  # Combine all columns into a dataframe
  df <- as.data.frame(columns)
  
  return(df)
}

# Example usage
set.seed(128)  # For reproducibility
test_df <- generate_test_dataframe(n_nominal = 2, n_ordinal = 2, n_continuous = 2, n_rows = 10)
print(test_df)

nominalvars_input <- c("nominal_1", "nominal_2", "nominal_3")
ordinalvars_input <- c("ordinal_1", "ordinal_2")
continuousvars_input <- c("continuous_1", "continuous_2")
modvars_input <- c("nominal_1", "nominal_2")

nominalvars <- advanced_match(nominalvars_input, test_df)
ordinalvars <- advanced_match(ordinalvars_input, test_df)
continuousvars <- advanced_match(continuousvars_input, test_df)
modvars <- advanced_match(modvars_input, test_df)

missing_vec <- c(0, 1, 3, 0, 0, 0, 0)

# for moderator vector, use column names instead, ditto for nominalvars, ordinalvars, etc.
cat("The modvars are:", modvars)
cat("The nominalvars are: ", nominalvars)
cat("The ordinalvars are: ", ordinalvars)
cat("The continuousvars are:", continuousvars)

mixed_df <- column_mixer_2(test_df, modvars, nominalvars, ordinalvars, continuousvars, missing_vec)
print(mixed_df)