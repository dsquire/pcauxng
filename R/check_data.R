check_data <- function (data, ignore = NULL) {
  if (length(datawizard::empty_rows(data)) > 0) {
    data <- datawizard::remove_empty_rows(data)
    message("We found empty rows in your data and removed them!")
  }

  empty_cols <- names(datawizard::empty_columns(data))

  if (length(empty_cols) > 0) {
    message("We found empty columns: ", toString(empty_cols))
  }

  character_cols <- names(data[, sapply(data, class) == "character"])

  if (length(character_cols) > 0) {
    message("We found character columns: ", toString(character_cols))
  }

  # Add check for columns with a variance of 0
  # build combined ignore here

  if (dimension_ratio(data, ignore) >= 1) {
    message("Your ratio of columns to rows is greater than or equal to 1")
  }


  ignore <- unique(c(ignore, empty_cols, character_cols))

  return(ignore)

}
