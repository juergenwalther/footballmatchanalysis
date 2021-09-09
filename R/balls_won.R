#' Erfassen aller Ballgewinne
#'
#' Definiert als erster Punkt in Passstafetten
#'
#' @param pass_norm data.frame mit Passpositionen normiert auf das Spielfeld
#'
#' @return data.frame with all balls gained
#' @export
#'
#' @examples
balls_won <- function(pass_norm) {
  df_ballswon <- pass_norm %>%
    group_by(Zeit) %>%
    summarise(
      winx = x[1],
      winy = y[1]
    )
  return(df_ballswon)
}
