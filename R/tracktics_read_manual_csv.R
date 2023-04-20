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

  colnames(ret) <- c("Spiel", "Anteil von Sprinten an Gesamtdistanz (in %)", "Anteil von Rennen an Gesamtdistanz (in %)",
                    "Anteil Spiel im vorderen Drittel", "Anzahl Sprints in Spielrichtung"
  )

  return(ret)
}
