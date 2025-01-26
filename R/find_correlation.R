# *****************************************************************************
#
# This function takes as input two columns (now called 'x' and 'y', as well
# as two variables that describe what data type we're dealing with.  Then, the
# appropriate correlation function is called.  A correlation is calculated and
# returned.
#                                                           BC 1/25/2025
# *****************************************************************************

source("convert_char_factor_to_numeric.R")
source("pearson.R")
source("kendall_tau.R")
source("rank_multiserial.R")
source("goodman_kruskall_lambda.R")

find_correlation <- function(x, y, type1, type2){
  
  # Convert the data to numeric if it isn't already
  if(is.character(x)){
    x <- convert_char_factor_to_numeric(x)
  }
  if(is.character(y)){
    y <- convert_char_factor_to_numeric(y)
  }
  
  
  # Define correlation type based on the pair of data types
  if (type1 == "continuous" & type2 == "continuous") {
    correl <- pearson(x, y)
    
  } else if ((type1 == "continuous" & type2 == "ordinal") || (type1 == "ordinal" & type2 == "continuous")) {
    correl <- kendall_tau(x, y)
    
  } else if ((type1 == "continuous" & type2 == "nominal") || (type1 == "nominal" & type2 == "continuous")) {
    correl <- rank_multiserial(x, y)
    
  } else if (type1 == "ordinal" & type2 == "ordinal") {
    correl <- kendall_tau(x, y)
    
  } else if ((type1 == "ordinal" & type2 == "nominal") || (type1 == "nominal" & type2 == "ordinal")) {
    correl <- rank_multiserial(x, y)
    
  } else if (type1 == "nominal" & type2 == "nominal") {
    correl <- goodman_kruskal_lambda(x, y)
  }
  
  return(correl)
}









