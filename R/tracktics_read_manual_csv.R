#' Extract parameters from manual Tracktics CSV file
#'
#' @param file string. full path to downloaded input csv from Tracktics "CSV Export"
#'
#' @return
#' @export data.frame with extracted parameters
#'
#' @examples
tracktics_read_manual_csv <- function(file) {
  ret <- read.csv(file = file)

  colnames(ret) <- c("Spiel", "Anteil an hoher Intensität an Gesamtdistanz (in %)", "Anteil von Sprinten an Gesamtdistanz (in %)", "Anteil von Rennen an Gesamtdistanz (in %)",
                    "Anteil Spiel im vorderen Drittel", "Tempowechsel insgesamt", "Anteil Tempowechsel Intensiv (in %)", "Anteil Tempowechsel Sprint (in %)",
                    "Anteil Tempowechsel Rennen (in %)", "Anzahl Sprints in Spielrichtung", "Anzahl Sprints gegen Spielrichtung", "Mittlere Sprintlänge (in m)"
  )

  return(ret)
}
