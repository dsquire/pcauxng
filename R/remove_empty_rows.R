remove_empty_rows <- function(data) {
  stopifnot(
    'argument "data" is missing, with no default' = !missing(data),
    'argument "data" must be a data frame' = is.data.frame(data)
  )
  data[rowSums(is.na(data)) != ncol(data), ]
}
