pcauxng <-
  function(data,
           miceargs1 = NULL,
           miceargs2 = NULL,
           prcompargs = NULL,
<<<<<<< HEAD

           # ignore = NULL,
           # quiet = FALSE) {
  )
=======
           ignore = NULL,
           quiet = FALSE) {
>>>>>>> b73688b562e834a7e9964830db78e1cdea71bfe6
    stopifnot(
      'argument "data" is missing, with no default' = !missing(data),
      'argument "data" must be a data frame' = is.data.frame(data),
      'argument "miceargs1" must be a list' = is.list(miceargs1),
      'argument "miceargs2" must be a list' = is.list(miceargs2),
      'argument "prcompargs" must be a list' = is.list(prcompargs)
    )

    # TODO: generate raw data with component scores and write out

    original_columns <- colnames(data)

    modifyList()

    ## TODO: Check for columns with no data and add list to ignore
    if (!is.null(ignore)) {
      ignoredata <- datawizard::get_columns(data, ignore)
      data <- datawizard::data_remove(data, select = ignore)
    }

    # get a shadow matrix to enable resetting the NA's instead of reloading
    # data

    miceargs1$data = data
    miceargs1$m = 1
    # rm(data)

    mice1 <- tryCatch({
      do.call(mice::mice, miceargs1)
    }, error = function(cnd) {
      message(conditionMessage(cnd))
    }, warning = function(cnd) {
      message(conditionMessage(cnd))
    } , finally = {
      # rm(data)
    })

    mice1
    # auxdata <- createauxvars(mice::complete(mice1), ignore, prcompargs)
    # remove(mice1)
    #
    # newdata <- cbind(data, auxdata)
    # newdata

  }
