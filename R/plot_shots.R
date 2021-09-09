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
plot_shots <- function(df,
                       spielrichtung = "r",
                       title = "Schüsse",
                       subtitle = paste0("Position der Schüsse im gesamten Spiel"),
                       outpath,
                       filename) {
  if (nrow(df) > 0) {
    soccerPitch(
      arrow = spielrichtung,
      title = title,
      subtitle = subtitle
    ) +
      geom_point(data = df, aes(x = x, y = y, color = Zeit), size = 3)


    ggsave(file = paste0(outpath, "/", filename, ".png"))
  }
}
