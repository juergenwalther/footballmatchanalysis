tracktics_read_player_name <- function(file) {
  df <- read.csv(file = file)
  df_names <- df %>%
    dplyr::select(First.Name, Last.Name) %>%
    dplyr::filter(nchar(First.Name) > 1)


  return(df_names)
}
