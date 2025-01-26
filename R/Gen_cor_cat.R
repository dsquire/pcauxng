

check_numeric_list <- function(lst) {
  for (elem in lst) {
    if (!is.numeric(elem)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

check_character_list <- function(lst) {
  for (elem in lst) {
    if (!(is.character(elem) || is.string(elem))) {
      return(FALSE)
    }
  }
  return(TRUE)
}

create_list_of_lists <- function(N, M) {
  # Create an empty list to store the result
  list_of_lists <- list()

  # Iterate over N to create N lists
  for (i in 1:N) {
    # Create a list containing M zeros and append it to the list_of_lists
    list_of_lists[[i]] <- rep(0, M)
  }
  return(list_of_lists)
}

swap <- function(data, i, j){
  temp1 <- data[i]
  temp2 <- data[j]
  data[j] <- temp1
  data[i] <- temp2
  return(data)
}

introduce_noise <- function(data1, data2, correl){
  tempCorr <- cor(data1, data2)
  x <- 0
  while((tempCorr > correl) && (x < 200)){
    random1 <- sample(1: length(data2), size = 1, replace = TRUE)
    random2 <- sample(1: length(data2), size = 1, replace = TRUE)
    data2 <- swap(data2, random1, random2)
    tempCorr <- cor(data1, data2)
    x <- x + 1
    if(x >= 200){
      print("The function 'introduce_noise' was unable to introduce enough noise to reduce the correlation down to the specified value")
    }
  }
  return(data2)
}

gen_cat_data <- function(data, correl, cat_vector){

  bool_data <- check_numeric_list(data)
  bool_cat_vector <- check_character_list(cat_vector)
  if((bool_data == FALSE) || (bool_cat_vector == FALSE)){
    print("Error: Either the data passed to gen_cat_data was not numeric or the cat_vector consisted of something other than caracters or strings.")
    stop()
  }


  N <- length(data)
  M <- 3

  vector1 <- data
  vector2 <- sample(1:length(cat_vector), size = length(data), replace = TRUE)
  vector1s <- sort(vector1)
  vector2s <- sort(vector2)

  vector3 <- create_list_of_lists(N, M)


  # This FOR loop assigns the original values in vector1
  # to the first element of each triad in the list of lists.
  # It also assigns the integers 1 through N to the third
  # element of each triad for unsorting later.
  for(i in (1:length(vector1))){
    temp1 <- vector1[i]
    vector3[[i]][[1]] <- temp1
    vector3[[i]][[3]] <- i
  }


  # this while loop sorts triads in the list of lists
  # based on the first element of each triad.
  out_of_order <- TRUE
  while(out_of_order == TRUE){

    out_of_order <- FALSE

    for(i in 1:(length(vector3)-1)){
      value1 <- vector3[[i]][[1]]
      value2 <- vector3[[i+1]][[1]]
      if(value1 > value2){
        out_of_order <- TRUE
        temp1 <- vector3[[i]]
        temp2 <- vector3[[i+1]]
        vector3[[i]] <- temp2
        vector3[[i+1]] <- temp1
      } # END IF
    } # END FOR
  } # END WHILE


  # Now that the list of lists is sorted based on element 1
  # of each triad, assign the sorted factor list to element 2
  # of each triad.
  for(i in (1:length(vector1))){
    tempVal <- vector2s[i]
    vector3[[i]][[2]] <- tempVal
  }


  # Now, sort the list of lists based on element 3.
  # This will restore the list of lists such that the
  # first element of each triad is in the original order,
  # but preserving the correlation between element 1 and 2 of each triad.

  out_of_order <- TRUE
  while(out_of_order == TRUE){

    out_of_order <- FALSE

    for(i in (1:(length(vector3)-1))){

      value1 <- vector3[[i]][[3]]

      value2 <- vector3[[i+1]][[3]]
      if(value1 > value2){

        out_of_order <- TRUE

        temp1 <- vector3[[i]]

        temp2 <- vector3[[i+1]]
        vector3[[i]] <- temp2
        vector3[[i+1]] <- temp1
      } # END IF
    } # END FOR
  } # END WHILE



  # Now, the first and second element of each triad should have the
  # needed correlation, and the first elements of each triad should
  # be restored to their original order.  This swatch of code unpacks
  # the first and second element of each triad into their own list
  # for further data processing.  This swatch of code also checks to
  # make sure the numbers of each list have the needed correlation.

  output1 <- list()
  output2 <- list()
  for(i in 1:length(vector1)){
    temp1 <- vector3[[i]][[1]]
    temp2 <- vector3[[i]][[2]]
    output1 <- append(output1, temp1)
    output2 <- append(output2, temp2)
  }

  # Now, check the correlation between output1 and output2
  tempOut1 <- unlist(output1)
  tempOut2 <- unlist(output2)
  print(tempOut1)
  print(tempOut2)
  tempCorr <- cor(tempOut1, tempOut2)

  # if the correlation between output1 and output2 is greater
  # that the correl specified by the user, introduce some noise
  if(tempCorr > correl){
      tempOut2 <- introduce_noise(tempOut1, tempOut2, correl)
  }
  return(tempOut2)
}

dataZ <- sample(1:100, size = 10, replace = TRUE)
dataA <- sort(dataZ)

list_of_cats <- list("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
myCorrel <- 0.7

cat_data <- gen_cat_data(dataZ, myCorrel, list_of_cats)

correlationX <- cor(dataZ, cat_data)
print(correlationX)
