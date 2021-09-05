#' Plot heatmap of all passes in 3 different ways
#'
#' @param df input data.frame of all passes
#' @param spielrichtung character vector of length 1.
#' Needs to be set to "r" for playing from left to right
#' @param plot_heatmap boolean to plot pass heatmap
#' @param plot_discrete boolean to plot pass descrete heatmap
#' @param plot_rawdata boolean to plot pass raw data
#'
#' @return no return object. A maximum of 3 plots are generated based
#' @export
#'
#' @examples
plot_pass_heatmap <- function(df, spielrichtung, plot_heatmap = T, plot_discrete = T, plot_rawdata = T) {
  if (plot_heatmap) {
    soccermatics:soccerHeatmap(df,
      lengthPitch = 105, widthPitch = 68, xBins = 20,
      yBins = 20, kde = TRUE, arrow = spielrichtung,
      colLow = "white", colHigh = "red", title = "Heatmap der Pässe",
      subtitle = "blau: wenig Pässe, rot: viele Pässe",
      x = "x", y = "y"
    )

    ggsave(file = paste0(outpath, "paesse_heatmap.png"))
  }

  if (plot_discrete) {
    soccermatics:soccerHeatmap(df,
      lengthPitch = 105, widthPitch = 68, xBins = 8,
      yBins = 8, kde = FALSE, arrow = spielrichtung,
      colLow = "white", colHigh = "red", title = "Heatmap der Pässe",
      subtitle = "Häufigkeit der Pässe anhand der Position im Feld",
      x = "x", y = "y"
    )

    ggsave(file = paste0(outpath, "paesse_heatmap_diskret.png"))
  }

  if (plot_rawdata) {
    soccermatics:soccerPitch(
      arrow = spielrichtung,
      title = "Passstafetten",
      subtitle = "Alle Passstafetten während des Spiels (farbcodiert nach gespielten Minuten)"
    ) +
      geom_path(
        data = pass_norm, aes(x = x, y = y, group = Zeit, color = Zeit),
        arrow = arrow(
          angle = 30, length = unit(0.05, "inches"),
          ends = arrhead,
          type = "closed"
        )
      )

    ggsave(file = paste0(outpath, "paesse_rawdata.png"))
  }
}
