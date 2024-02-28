check_data <- function {

  empty_columns <-
  empty_rows <- length(datawizard::empty_rows(data))


  # TODO: remove `ignore` from dimension_ratio
  if (dimension_ratio(data, ignore) >= 1) {
    message("Your ratio of columns to rows is greater than or equal to 1")
  }


}


a <- c("red", "blue", "green")
b <- c("red", "yellow")
c <- c(a, b)
unique(c)

modifyList(a, b)
toString(c)
