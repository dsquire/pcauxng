#' @description
#' Return a list of `mice` arguments.
#' @return a named list of `mice` arguments.
#' @importFrom mice mice
#' @export getmiceargs
getmiceargs <- function() {
  miceargs <- as.list(formals(fun = mice::mice))

  # Strip off the `...` function parameter
  miceargs[length(miceargs)] <- NULL
  miceargs
}
