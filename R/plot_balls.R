#' Title
#'
#' @param df
#' @param spielrichtung
#' @param title
#' @param subtitle
#' @param outpath
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
plot_balls <- function(df,
                       spielrichtung = "r",
                       title = "Ballgewinne",
                       subtitle = "Position der Ballgewinne (farbcodiert nach gespielten Minuten)",
                       outpath,
                       filename) {
  soccerPitch(
    arrow = spielrichtung,
    title = title,
    subtitle = subtitle
  ) +
    geom_point(data = df, aes(x = winx, y = winy, color = Zeit), size = 3)


  ggsave(file = paste0(outpath, "/", filename, ".png"))
}
