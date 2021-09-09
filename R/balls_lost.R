#' Erfassen aller Ballverluste
#'
#' Definiert als erster Punkt in Passstafetten
#'
#' @param pass_norm data.frame mit Passpositionen normiert auf das Spielfeld
#'
#' @return data.frame with all balls gained
#' @export
#'
#' @examples
balls_lost <- function(pass_norm) {
  df_ballslost <- pass_norm %>%
    group_by(Zeit) %>%
    summarise(
      winx = x[length(x)],
      winy = y[length(y)]
    )
  return(df_ballslost)
}
