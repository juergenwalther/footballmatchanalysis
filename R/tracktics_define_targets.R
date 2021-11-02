#' Define targets of parameters coming from Tracktics
#'
#' @param df data.frame with first column containing parameter names
#'
#' @return character vector with goals of length of number of rows of df
#' @export
#'
#' @examples
tracktics_define_targets <- function(df){
  targets <- rep("-", dim(df)[1])
  targets[df[,1] == "Durschnittliche Distanz pro Spieler (in km)"] <- " Je nach Position 9 - 10km"
  targets[df[,1] == "Anteil von HI an Gesamtdistanz (in %)"] <- "27% + x"
  targets[df[,1] == "Anteil von Sprinten an Gesamtdistanz (in %)"] <- "5% + x"
  targets[df[,1] == "Durchschnittliche Anzahl Sprints pro Spieler"] <- "Je nach Position 25 + x"

  return(targets)
}
