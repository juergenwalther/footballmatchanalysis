#' Plot heatmap of all passes in 3 different ways
#'
#' @param df input data.frame of all passes
#' @param spielrichtung character vector of length 1.
#' Needs to be set to "r" for playing from left to right
#' @param plot_heatmap boolean to plot pass heatmap
#' @param plot_discrete boolean to plot pass descrete heatmap
#' @param plot_rawdata boolean to plot pass raw data
#' @param outpath character string. Output path for analysis pictures
#' @param name_heatmap character string. filename of heatmap plot
#' @param name_discrete character string. filename of discrete heatmap plot
#' @param name_raw character string. filename of raw data plot
#'
#' @return no return object. A maximum of 3 plots are generated based on the boolean indicators
#' in the function arguments
#' @export
#'
#' @examples
plot_pass_heatmap <- function(df, outpath, spielrichtung = "r", plot_heatmap = T, plot_discrete = T, plot_rawdata = T,
                              title_heatmap = "Heatmap der Pässe", subtitle_heatmap = "blau: wenig Pässe, rot: viele Pässe",
                              title_discrete = "Heatmap der Pässe", subtitle_discrete = "Häufigkeit der Pässe anhand der Position im Feld",
                              title_raw = "Passstafetten", subtitle_raw = "Alle Passstafetten während des Spiels (farbcodiert nach gespielten Minuten)",
                              name_heatmap = "paesse_heatmap", name_discrete = "paesse_heatmap_diskret",
                              name_raw = "paesse_rawdata") {
  if (plot_heatmap) {
    soccerHeatmap(df,
      lengthPitch = 105, widthPitch = 68, xBins = 20,
      yBins = 20, kde = TRUE, arrow = spielrichtung,
      colLow = "white", colHigh = "red", title = title_heatmap,
      subtitle = subtitle_heatmap,
      x = "x", y = "y"
    )

    ggsave(file = paste0(outpath, "/", name_heatmap, ".png"))
  }

  if (plot_discrete) {
    soccerHeatmap(df,
      lengthPitch = 105, widthPitch = 68, xBins = 8,
      yBins = 8, kde = FALSE, arrow = spielrichtung,
      colLow = "white", colHigh = "red", title = title_discrete,
      subtitle = subtitle_discrete,
      x = "x", y = "y"
    )

    ggsave(file = paste0(outpath, "/", name_discrete, ".png"))
  }

  if (plot_rawdata) {
    soccerPitch(
      arrow = spielrichtung,
      title = title_raw,
      subtitle = subtitle_raw
    ) +
      geom_path(
        data = df, aes(x = x, y = y, group = Zeit, color = Zeit),
        arrow = arrow(
          angle = 30, length = unit(0.05, "inches"),
          ends = arrhead,
          type = "closed"
        )
      )

    ggsave(file = paste0(outpath, "/", name_raw, ".png"))
  }
}
