summary_table <- function(pass, shots, own_area = 22, own_half = 52.5, opponent_area = 105-16) {
  df_balls_lost <- balls_lost(pass)
  df_balls_won <- balls_won(pass)

  n_pass <- nrow(pass)
  n_pass_more10 <- nrow(get_pass_sequence(pass, n = 10))
  n_pass_more20 <- nrow(get_pass_sequence(pass, n = 20))
  n_shots <- nrow(shots)
  n_shots_inside_area <- shots %>% filter(x > opponent_area)
  n_shots_outside_area <- n_shots - n_shots_outside_area
  n_ballslost_1 <- df_balls_lost %>% filter(x < own_area)
  n_ballslost_2 <- df_balls_lost %>% filter(x < own_half)
  n_ballslost_3 <- nrow(df_balls_lost) - n_ballslost_2
  n_ballswon_1 <- df_balls_won %>% filter(x < own_area)
  n_ballswon_2 <- df_balls_won %>% filter(x < own_half)
  n_ballswon_3 <- nrow(df_balls_won) - n_ballswon_2

  result <- c(n_pass, n_pass_more10, n_pass_more20, n_shots, n_shots_inside_area, n_shots_outside_area,
              n_ballslost_1, n_ballslost_2, n_ballslost_3, n_ballswon_1, n_ballswon_2, n_ballswon_3)

  myrownames <- c("Pässe", "Passstafetten mit mehr als 10 Pässen","Passstafetten mit mehr als 20 Pässen",
                 "Schüsse", "Schüsse innerhalb des Strafraums", "Schüsse außerhalb des Strafraums", "Ballverlust in und um eigenen Strafraum",
                 "Ballverlust in eigener Hälfte", "Ballverlust in gegnerischer Hälfte", "Ballgewinn in und um eigenen Strafraum",
                 "Ballgewinn in eigener Hälfte", "Ballgewinn in gegnerischer Hälfte")

  df_res <- cbind(myrownames, result)
  colnames(df_res) <- c("Parameter", "Anzahl")
}
