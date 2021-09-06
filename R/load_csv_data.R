#' Load a csv file of a path analysis via GIMP
#'
#' @param i integer value of number of section to load data from
#' @param filename character string. Filename is either "paesse", "schuesse" or "schuesse_gegner"
#' @param spiel charcater string. Folder name of match where video analysis is saved
#' @param path character string. Parent path of spiel folder
#'
#' @return
#' @export
#'
#' @examples
load_csv_data <- function(i, filename, spiel, path) {
  complete_filename <- paste0("C://Users/Juergis/Documents/Fussball/Spielanalyse/Programm/Videoanalyse/WFV/", spiel, "/", filename, "_", i, ".csv")
  if (file.exists(complete_filename)) {
    df <- fread(file = complete_filename)
  } else {
    df <- data.frame()
  }
  return(df)
}
