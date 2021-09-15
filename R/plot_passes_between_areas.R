plot_passes_between_areas <- function(df,
                                      spielrichtung = "r",
                                      pos1,
                                      pos2,
                                      title = "Pässe von Rechtsverteidiger zu rechtem Mittelfeld",
                                      subtitle = "Gesamtes Spiel",
                                      outpath,
                                      filename) {
  if (!is.null(df)) {
    soccerPitch(
      arrow = spielrichtung,
      title = title,
      subtitle = subtitle
    ) +
      geom_path(data = df, aes(x = x, y = y, group = Zeit, color = Zeit), arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last", type = "closed")) +
      geom_rect(data = pos1, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), color = "red", alpha = 0.05) +
      geom_rect(data = pos2, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), color = "darkgreen", alpha = 0.05)


    ggsave(file = paste0(outpath, "/", filename, ".png"))
  }
}
