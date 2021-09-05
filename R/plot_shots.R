plot_shots <- function(df, outpath, filename) {
  soccermatics:soccerPitch(
    arrow = spielrichtung,
    title = "Schüsse",
    subtitle = "Position der abgegebenen Schüsse (farbcodiert nach gespielten Minuten)"
  ) +
    geom_point(data = schuss_norm, aes(x = x, y = y, color = Zeit), size = 3)


  ggsave(file = paste0(outpath, filename, ".png"))
}
