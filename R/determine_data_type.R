# ******************************************************************************
#
# This function takes a column number (integer) and determines if that integer
# is in the vectors 'nominalvars', 'ordinalvars', etc.
#
# ******************************************************************************
determine_data_type <- function(i, nominalvars, ordinalvars, continuousvars){
  
  if(i %in% nominalvars){
    type <- "nominal"
  } else if(i %in% ordinalvars){
    type <- "ordinal"
  } else if(i %in% continuousvars){
    type <- "continuous"
  }
  return(type)
}