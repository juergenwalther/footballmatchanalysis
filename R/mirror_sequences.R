#' Mirror the points at center of the pitch
#'
#' Die Spielrichtung wird gespiegelt, wenn von rechts nach links gespielt wird.
#' Standardrichtung wird von links nach rechts sein
#'
#' @param x numeric x-coordinate to mirror
#' @param center numeric x-coordinate of center of pitch (kickoff point)
#'
#' @return numeric mirrored coordinate
#' @export
#'
#' @examples
mirror_pitch <- function(x, center) {
  return(ifelse(x >= center, x - 2 * (x - center), x + 2 * (abs(x - center))))
}

#' Mirror sequences
#'
#' @param df_norm Normalized sequence of passes data.fram
#' @param spielrichtung_real Direction of play. Options are "l" for playing
#' from right to left, "r" when playing from left to right
#'
#' @return data.frame with mirrored passes
#' @export
#'
#' @examples
mirror_sequences <- function(df_norm, spielrichtung_real) {
  if (nrow(df_norm) > 0 & isTRUE(spielrichtung_real == "l")) {
    df_mirrored <- df_norm %>%
      dplyr::mutate(
        x = mirror_pitch(x, 52.5),
        y = mirror_pitch(y, 34)
      )
  } else {
    df_mirrored <- df_norm
  }

  return(df_mirrored)
}
