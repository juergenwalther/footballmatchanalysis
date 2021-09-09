#' Define aread on pitch for further analysis
#'
#' Das Spielfeld ist 105 lang und 68 breit
#'
#'
#' @return list of data.frames with coordinates defining rectangular areas
#' (x1,x2,y1,y2) (x1,y1) sind die Koordinaten des Punktes links unten im Rechteck
#' (x2,y2) sind die Koordinaten rechts oben im Rechteck
#'
#' @export
#'
#' @examples
define_areas <- function(){

  #(x1,x2,y1,y2) (x1,y1) sind die Koordinaten des Punktes links unten im Rechteck, (x2,y2) sind die Koordinaten rechts oben im Rechteck. Das Spielfeld ist 105 lang und 68 breit
    rv <- data.frame(x1 = 0, x2 = 45, y1 = 0, y2 = 14)
    lv <- data.frame(x1 = 0, x2 = 45, y1 = 54, y2 = 68)
    rm <- data.frame(x1 = 70, x2 = 105, y1 = 0, y2 = 14)
    lm <- data.frame(x1 = 70, x2 = 105, y1 = 54, y2 = 68)
    mittelkreis = data.frame(x1 = 30, x2 = 75, y1 = 18, y2 = 50)

    return(list(rv = rv, lv = lv, m = mittelkreis, rm = rm, lm = lm))
}
