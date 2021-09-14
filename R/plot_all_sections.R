#' Title
#'
#' @param spielrichtung
#' @param title
#' @param subtitle
#' @param outpath
#' @param filename
#' @param list_dfs
#' @param n_sections
#'
#' @return
#' @export
#'
#' @examples
plot_all_sections <- function(list_dfs,
                       spielrichtung = "r",
                       n_sections,
                       parameter,
                       size_points = 2,
                       title = "Schüsse",
                       subtitle = paste0("Position der Schüsse im gesamten Spiel"),
                       outpath,
                       filename) {

  p <- list()

  if(parameter == "shots"){

  for(i in 1:n_sections){
    df <- list_dfs[[i]]
    if (nrow(df) > 0) {

    p[[i]] <- soccerPitch(
        arrow = "none",
      ) + geom_point(data = df, aes(x = x, y = y), size = 2.5)
    } else {
      p[[i]] <- soccerPitch(
        arrow = "none",
      ) +
        geom_point(data = data.frame(x = 52.5, y = 34, Zeit = 1), aes(x = x, y = y), size = size_points)
    }
  }
  } else if(parameter == "balls"){
    for(i in 1:n_sections){
      df <- list_dfs[[i]]
      if (nrow(df) > 0) {

        p[[i]] <- soccerPitch(
          arrow = "none",
        ) + geom_point(data = df, aes(x = winx, y = winy), size = 2.5)
      } else {
        p[[i]] <- soccerPitch(
          arrow = "none",
        ) +
          geom_point(data = data.frame(x = 52.5, y = 34, Zeit = 1), aes(x = x, y = y), size = size_points)
      }
  }
  }

 p <- grid.arrange(arrangeGrob(p[[1]], top = "Spielabschnitt 1"),
                   arrangeGrob(p[[2]], top = "Spielabschnitt 2"),
                   arrangeGrob(p[[3]], top = "Spielabschnitt 3"),
                   arrangeGrob(p[[4]], top = "Spielabschnitt 4"),
                   arrangeGrob(p[[5]], top = "Spielabschnitt 5"),
                   arrangeGrob(p[[6]], top = "Spielabschnitt 6"),
                   nrow = 3, ncol = 2,
                   top = textGrob(title,gp=gpar(fontsize=25)))


    ggsave(plot = p, filename = paste0(outpath, "/", filename, ".png"), width = 12, height = 11)

}
