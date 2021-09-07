#' Erfassen aller Ballgewinne in der gegnerischen Hälfte
#'
#' Ballgewinne ist definiert als erster Punkt in Passstafetten
#'
#' @param pass_norm data.frame mit Passpositionen normiert auf das Spielfeld
#' @param spielrichtung character vector. either "l" or "r"
#'
#' @return data.frame with all balls gained
#' @export
#'
#' @examples
high_pressing <- function(pass_norm, spielrichtung) {
  if (strcmp(spielrichtung, "l")) {
    df_highpressing <- pass_norm %>%
      group_by(Zeit) %>%
      filter(x[1] < 53)
  } else {
    df_highpressing <- pass_norm %>%
      group_by(Zeit) %>%
      filter(x[1] > 53)
  }
  return(df_highpressing)
}
