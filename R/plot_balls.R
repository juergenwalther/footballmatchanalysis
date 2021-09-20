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
    geom_point(data = df, aes(x = winx,
                              y = winy,
                              color = Zeit
                              #colour = cut(
                                #Zeit, c(-Inf, 299, 499, 699, Inf))
                                #Zeit, c(-Inf, 399, Inf))
                              ),
               size = 3)
#scale_color_manual(name = "Zeit",
                   # values = c("(-Inf,399]" = "black",
                   #            "(399, Inf]" = "green"),
                   # labels = c("1", "2")
                   #)


  ggsave(file = paste0(outpath, "/", filename, ".png"))
}
