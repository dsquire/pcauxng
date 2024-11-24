

determine_data_pairing <- function(var1, var2){

  pairing <- switch(var1,
                   "continuous" = switch(var2,
                                         "continuous" = 1,
                                         "ordinal" = 2,
                                         "nominal" = 3,
                                         "unknown" = 4),
                   "ordinal" = switch(var2,
                                      "continuous" = 5,
                                      "ordinal" = 6,
                                      "nominal" = 7,
                                      "unknown" = 8),
                   "nominal" = switch(var2,
                                      "continuous" = 9,
                                      "ordinal" = 10,
                                      "nominal" = 11,
                                      "unknown" = 12),
                   "unknown" = switch(var2,
                                      "continuous" = 13,
                                      "ordinal" = 14,
                                      "nominal" = 15,
                                      "unknown" = 16))
  return(pairing)
}

# Example of switch over two variables in R using nested switch statements
var1 <- "ordinal"
var2 <- "continuous"

result <- determine_data_pairing(var1, var2)
print(result)