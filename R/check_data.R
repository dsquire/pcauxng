check_data <- function (data) {
  empty_rows <- datawizard::empty_rows(data)

  if (length(empty_rows) > 0) {
    message("We found empty rows: ", toString(empty_rows))
  }

  empty_cols <- names(datawizard::empty_columns(data))

  if (length(empty_cols) > 0) {
    message("We found empty columns: ", toString(empty_cols))
  }

  character_cols <-
    names(data[, sapply(data, class) == "character"])

  if (length(character_cols) > 0) {
    message("We found character columns: ", toString(character_cols))
  }

  nzv_cols <- caret::nzv(data, names = TRUE)

  if (length(nzv_cols) > 0) {
    message(
      "We found columns with zero variance and / or near-zero variance: ",
      toString(nzv_cols)
    )
  }

  # ignore <- unique(c(ignore, empty_cols, character_cols, nzv_cols))

  # if (dimension_ratio(data, ignore) >= 1) {
  #   message("Your ratio of columns to rows is greater than or equal to 1")
  # }

  checklist <- list(
    empty_rows = empty_rows,
    empty_cols = empty_cols,
    character_cols = character_cols,
    nzv_cols = nzv_cols
  )

  return(checklist)

}
