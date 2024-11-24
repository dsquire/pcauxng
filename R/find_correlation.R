

source("convert_char_factor_to_numeric.R")
source("pearson.R")
source("kendall_tau.R")
source("rank_multiserial.R")
source("goodman_kruskall_lambda.R")

find_correlation <- function(x, y){
  
  # determine what data type each column is
  type1 <- x[1]
  type2 <- y[1]
  
  
  # remove the entry containing the type of data
  x <- x[-1]
  y <- y[-1]
  
  # Convert the data to numeric if it isn't already
  if(is.character(x)){
    x <- convert_char_factor_to_numeric(x)
  }
  if(is.character(y)){
    y <- convert_char_factor_to_numeric(y)
  }
  
  
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
    correl <- pearson(x, y)
    
  } else if (correlation_type == "kendall") {
    correl <- kendall_tau(x, y)
    
  } else if (correlation_type == "rank_multiserial") {
    correl <- rank_multiserial(x, y)
    
  } else if (correlation_type == "rank_multiserial") {
    correl <- rank_multiserial(x, y)
    
  } else if (correlation_type == "goodman_kruskal_lambda") {
    correl <- goodman_kruskal_lambda(x, y)
    
  }
  return(correl)
}









