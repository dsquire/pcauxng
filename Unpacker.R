

generate_output <- function(a, b, c) {
  # Split the range specified in 'a' into individual numbers
  charX <- '-'
  result <- unlist(strsplit(a, charX))
  range1 <- as.numeric(result[1])
  range2 <- as.numeric(result[2])

  # Generate the sequence of numbers from 'a'
  numbers <- seq(range1, range2)

  # Create the output list
  output <- list()
  for (num in numbers) {
    output[[length(output) + 1]] <- c(num, b, c)
  }

  return(output)
}

unpack_cat <- function(data){
  output <- list()
  element <- c(0, 0, 0)
  for(i in 1:length(data)){
    output[i] <- c(output, element)
  }

  for(i in 1:length(data)){
    a <- data[[i]][[1]]
    b <- data[[i]][[2]]
    c <- data[[i]][[3]]
    triad <- generate_output(a, b, c)

    output[[i]] <- triad

  }
  return(output)
}

dat <- list(   list("10-13", 1, 0.8),   list("14-20", 3, 0.7),   list("21-25", 5, 0.4)     )
unpacked_dat <- unpack_cat(dat)


