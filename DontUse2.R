

Get_distance <- function(x1, y1, x2, y2){
  
  x <- (x2-x1)^2
  y <- (y2-y1)^2
  
  distance <- sqrt(x + y)
  return(distance)

}