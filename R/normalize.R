#' Normalize in x direction
#'
#' Unten links auf dem Spielfeld ist die Koordinate (0,0).
#' Die Koordinaten haben ihren Maximalwert in x- und y- Richtung
#' von 100, also die Eckfahne rechts oben ist (100,100)
#' Größe von meinem Spielfeld 1136x782
#'
#' @param x numeric value to apply normalization to
#' @param num numeric value to normalize on (usually set to 105)
#'
#' @return normalized numeric value
#' @export
#'
#' @examples
normalize_x = function(x,num){
  xnew = x/1136*num
  return(xnew)
}



#' Normalize in y direction
#'
#' Unten links auf dem Spielfeld ist die Koordinate (0,0).
#' Die Koordinaten haben ihren Maximalwert in x- und y- Richtung
#' von 100, also die Eckfahne rechts oben ist (100,100)
#' Größe von meinem Spielfeld 1136x782
#'
#' @param y numeric value to apply normalization to
#' @param num numeric value to normalize on (usually set to 68)
#'
#' @return normalized numeric value
#' @export
#'
#' @examples
normalize_y = function(y,num){
  ynew = y/782*num
  ynew = abs(ynew-num)
  return(ynew)
}
