
#source("Gen_cor_cat.R")

get_first_number <- function(u_input_string) {

  charX <- '-'
  input_string <- unlist(u_input_string)

  result <- unlist(strsplit(input_string, charX))

  # Convert the first part to a number
  first_number <- as.numeric(result[1])

  return(first_number)
}

get_last_number <- function(u_input_string) {

  charX <- '-'
  input_string <- unlist(u_input_string)

  result <- unlist(strsplit(input_string, charX))

  # Convert the first part to a number
  last_number <- as.numeric(result[2])

  return(last_number)
}

unpack_cont <- function(data){
  output <- c()

  # check for hyphenated entries:
  for(i in 1:length(data)){
    u <- data[i]
    if(grepl("-", u)){

      firstNumber <- get_first_number(u)
      lastNumber <- get_last_number(u)
      for(j in (firstNumber:lastNumber)){
        output <- append(output, j)
      }
    }
    else{
      output <- append(output, data[i])
    }
  }
  return(output)
}

compare_lists <- function(list1, list2) {
  any_common_elements <- any(list1 %in% list2)

  return(any_common_elements)
}

generate_data_frame <- function(nrows, ncols, u_numeric_cols, u_factor_cols){

  numeric_cols <- unpack(u_numeric_cols)
  factor_cols <- unpack(u_factor_cols)
  bool <- compare_lists(numeric_cols, factor_cols)
  if(bool == TRUE){
    print("Error: At least one column was specified to be both a numeric column and a factor column.")
    stop()
  }
  print(numeric_cols)
  print(factor_cols)

  return(1)
}

df <- generate_data_frame(10, 4, nc, nl, NULL)
print(df)

