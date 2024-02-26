pcauxng <-
  function(data,
           miceargs1 = NULL,
           miceargs2 = NULL,
           prcompargs = NULL,
           ignore = NULL,
           quiet = FALSE) {
    stopifnot(
      'argument "data" is missing, with no default' = !missing(data),
      'argument "data" must be a data frame' = is.data.frame(data)
      # TODO: Add checks for miceargs and prcompargs
    )

    original_columns <- colnames(data)

    empty_columns <- names(datawizard::empty_columns(data))
    empty_rows <- length(datawizard::empty_rows(data))

    ## TODO: Check for columns with no data and add list to ignore
    if (ignore != NULL) {
      ignoredata <- datawizard::get_columns(data, ignore)
      data <- datawizard::data_remove(data, select = ignore)
    }


    # TODO: remove `ignore` from dimension_ratio
    if (dimension_ratio(data, ignore) >= 1) {
      message("Your ratio of columns to rows is greater than or equal to 1")
    }

    # get a shadow matrix to enable resetting the NA's instead of reloading
    # data

    miceargs1$data = data
    miceargs1$m = 1
    # rm(data)

    mice1 <- tryCatch({
      do.call(mice::mice, miceargs1)
    },
    error = function(cnd) {
      message(conditionMessage(cnd))
    },
    warning = function(cnd) {
      message(conditionMessage(cnd))
    } ,
    finally = {
      # rm(data)
    })

    mice1
    # auxdata <- createauxvars(mice::complete(mice1), ignore, prcompargs)
    # remove(mice1)
    #
    # newdata <- cbind(data, auxdata)
    # newdata

  }
