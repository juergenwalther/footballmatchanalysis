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
                       title = "Schüsse",
                       subtitle = paste0("Position der Schüsse im gesamten Spiel"),
                       outpath,
                       filename) {

  p <- list()

  for(i in 1:n_sections){

    df <- list_dfs[[i]]

    if (nrow(df) > 0) {

    p[[i]] <- soccerPitch(
        arrow = "none",
      ) + geom_point(data = df, aes(x = x, y = y), size = 1.2)


    } else {
      p[[i]] <- soccerPitch(
        arrow = "none",
      ) +
        geom_point(data = data.frame(x = 52.5, y = 34, Zeit = 1), aes(x = x, y = y), size = 1.2)
    }
  }

 p <- grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],
                   nrow = 3, ncol = 2,
                   top = textGrob(title,gp=gpar(fontsize=20,font=3)))

    ggsave(plot = p, filename = paste0(outpath, "/", filename, ".png"), width = 10, height = 9)

}
