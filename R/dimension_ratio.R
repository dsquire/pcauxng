#' @description
#' Calculate the ratio of columns to rows in a data.frame.
#' @param data A data.frame, with two dimensions.
#' @param ignore A character vector of column names to exclude from the calculation.
#' @return a numeric ratio
#' @export dimension_ratio
dimension_ratio <-
  function(data, ignore = NULL) {
    stopifnot("`data` must be a data.frame" = is.data.frame(data))

    if (is.null(ignore)) {
      ncol(data) / nrow(data)
    } else {
      stopifnot(
        "`ignore` must be a character vector" = is.character(ignore),
        "`ignore` must have at least 1 element" = length(ignore) >= 1,
        "At least one element in 'ignore' does not match the columns in `data`" = all(ignore %in% colnames(data))
      )
      ncol(data[, !(colnames(data) %in% ignore)]) / nrow(data)
    }
  }
