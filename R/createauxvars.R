createauxvars <- function(data, ignore, prcompargs) {
  df <- data[,!(names(data) %in% ignore)]
  remove(data)

  dfcombn <- combn(df, m = 2, simplify = FALSE)
  auxlen = length(dfcombn)
  remove(df)

  dfauxvars <- lapply(
    dfcombn,
    FUN = function(x) {
      x[, 1] * x[, 2]
    }
  )

  result <- as.data.frame(matrix(unlist(dfauxvars),
                                 ncol = auxlen,
                                 byrow = TRUE))

  result


  # danny has combination function

  # pcaOut <- prcomp(data, scale = TRUE, retx  = TRUE)
  # d <- pcaOut$x

  # for the principle components do we
  # center ?
  # scale ?


  # merged_data <- cbind(data, d)
  # merged_data
}
