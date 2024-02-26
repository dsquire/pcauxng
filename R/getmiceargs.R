getmiceargs <- function() {
  miceargs <- as.list(formals(fun = mice::mice))

  # Strip off the `...` function parameter
  miceargs[length(miceargs)] <- NULL
  miceargs
}
