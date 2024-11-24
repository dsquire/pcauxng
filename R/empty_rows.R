empty_rows <- function(data) {
  stopifnot(
    'argument "data" is missing, with no default' = !missing(data),
    'argument "data" must be a data frame' = is.data.frame(data)
  )
  as.numeric(row.names(data[rowSums(is.na(data)) == ncol(data), ]))
}
