plot_won_space <- function(pass, histogram = FALSE, outpath, filename) {
  if (histogram == FALSE) {
    # Basic scatter plot
    ggplot(pass, aes(x = space_won, y = total, color = space_won)) +
      geom_point(shape = 16, size = 5, show.legend = FALSE) +
      # geom_smooth(method=lm, show.legend = FALSE) +
      theme_minimal() +
      scale_color_gradient(low = "#0091ff", high = "#f0650e") +
      xlim(-50, 80) +
      ylim(0, 15) +
      labs(
        title = "Raumgewinn vs Anzahl der Pässe in Stafette",
        x = "Raumgewinn (in % von Spielfeldlänge)",
        y = "Anzahl Pässe in Stafette"
      )


    ggsave(file = paste0(outpath, "/", filename, ".png"))
  }
  else {
    ggplot(pass, aes(x = space_won)) +
      geom_histogram(breaks = c(-75,-50,-25,0,25,50,75,100), color = "black", fill = "blue") +
      theme_minimal() +
      labs(
        title = "Raumgewinn im Histogram",
        x = "Raumgewinn (in % von Spielfeldlänge)",
        y = "Anzahl Passstafetten"
      ) +
      ylim(0, 50)

    ggsave(file = paste0(outpath, "/", filename, ".png"))
  }
}
