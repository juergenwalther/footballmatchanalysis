plot_won_space <- function(pass, histogram = FALSE, outpath, filename) {
  if (histogram == FALSE) {
    # Basic scatter plot
    ggplot(pass, aes(x = space_won, y = total, color = as.factor(area))) +
      geom_point(shape = 16, size = 5) +
      # geom_smooth(method=lm, show.legend = FALSE) +
      theme_minimal() +
      #scale_color_gradient(low = "#0091ff", high = "#f0650e") +
      xlim(-50, 80) +
      ylim(0, 15) +
      labs(
        title = "Raumgewinn vs Anzahl der Pässe in Stafette",
        x = "Raumgewinn (in % von Spielfeldlänge)",
        y = "Anzahl Pässe in Stafette"
      ) +
      scale_color_manual(name = "Spielfeld Drittel",
                         values = c("1" = "red",
                                    "2" = "green",
                                    "3" = "blue"),
                         labels = c("defensiv", "mitte", "offensiv"))


    ggsave(file = paste0(outpath, "/", filename, ".png"))
  }
  else {
    ggplot(pass, aes(x = space_won, y=..count../sum(..count..), fill = as.factor(area))) +
      #geom_bar(aes(y = (..count..)/sum(..count..)), breaks = c(-75,-50,-25,0,25,50,75,100), color = "black", fill = "blue") +
      #scale_y_continuous(formatter = 'percent') +
      geom_histogram(breaks = c(-75,-50,-25,0,25,50,75,100)) +
      theme_minimal() +
      labs(
        title = "Raumgewinn im Histogram",
        x = "Raumgewinn (in % von Spielfeldlänge)",
        y = "Anteil Passstafetten an Gesamtzahl von Passstafetten"
      ) +
      ylim(0, 0.5) +
      scale_fill_manual(name = "Spielfeld Drittel",
                         values = c("1" = "red",
                                    "2" = "green",
                                    "3" = "blue"),
                         labels = c("defensiv", "mitte", "offensiv"))

    ggsave(file = paste0(outpath, "/", filename, ".png"))
  }
}
