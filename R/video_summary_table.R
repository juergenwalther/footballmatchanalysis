<<<<<<< HEAD
<<<<<<< HEAD:R/sporttotal_summary_table.R
sporttotal_summary_table <- function(pass,
=======
=======
#' Summary table of video analysis
#'
#' @param pass data.frame with to meter normalized passes
#' @param shots data.frame with position of shots
#' @param shots_opp data.frame with position of shots of opponent
#' @param pass_areas data.fram with passes in areas
#' @param pass_space_won_tot data.frame with total won space of passes
#' @param ecken integer number of corners of WFV
#' @param ecken_gegner integer number of corners of opponent
#' @param own_area integer to define border of own area
#' @param own_half integer to define border of own half
#' @param opponent_area integer to define border of opponent area
#'
#' @return data.frame with summary statistics
#' @export
#'
#' @examples
>>>>>>> 30c107d... Obtain pass distance
video_summary_table <- function(pass,
>>>>>>> 9615926... Calculate pass distance:R/video_summary_table.R
                          shots,
                          shots_opp,
                          pass_areas,
                          pass_space_won_tot,
                          ecken,
                          ecken_gegner,
                          own_area = 22,
                          own_half = 52.5,
                          opponent_area = 105-16) {
  df_balls_lost <- balls_lost(pass)
  df_balls_won <- balls_won(pass)
  df_pass_dist <- get_pass_distance(pass = pass)
  #n_pass <- get_all_passes_completed(pass)
  n_pass <- nrow(pass)
  # Pässe in einer Halbzeit
  n_pass_1half <- pass %>% dplyr::filter(Zeit < 500) %>% nrow()
  n_pass_2half <- pass %>% dplyr::filter(Zeit > 500) %>% nrow()
  possession <- round((0.001336 * n_pass - 0.194287)*100,0)
  # Pässe pro Passstafette
  n_pass_in_sequence <- get_average_passes_in_sequence(pass = pass)
  # Passstafetten mit mehr als x Pässen
  n_pass_more10 <- get_pass_sequences(pass, n = 10) %>% ungroup() %>% dplyr::select(Zeit) %>% dplyr::distinct() %>% summarise(n())
  n_pass_more20 <- get_pass_sequences(pass, n = 20) %>% ungroup() %>% dplyr::select(Zeit) %>% dplyr::distinct() %>% summarise(n())
  # Passstafetten mit Raumgewinn
  n_pass_s5_spw50 <- nrow(pass_space_won_tot[pass_space_won_tot$space_won >= 50 & pass_space_won_tot$total < 5,])
  n_pass_l5_spw50 <- nrow(pass_space_won_tot[pass_space_won_tot$space_won >= 50 & pass_space_won_tot$total >= 5,])
  n_pass_l10_spw50 <- nrow(pass_space_won_tot[pass_space_won_tot$space_won >= 50 & pass_space_won_tot$total >= 10,])
  n_pass_area <- round(get_passes_completed_areas(pass)$perc, 2)
  n_pass_area_all <- round(get_passes_completed_areas(pass)$all_perc[1], 2)
  # Schüsse
  n_shots <- nrow(shots)
  n_shots_inside_area <- shots %>% filter(x > opponent_area) %>% summarise(n())
  n_shots_outside_area <- n_shots - n_shots_inside_area
  n_shots_opp <- nrow(shots_opp)
  n_shots_opp_inside_area <- shots_opp %>% dplyr::filter(x < own_area) %>% summarise(n())
  n_shots_opp_outside_area <- n_shots_opp - n_shots_opp_inside_area
  # Anzahl gewonnener und verlorener Bälle nach Gebiet (1: eigener Strafraum, 2: eigene Hälfte, 3: gegnerische Hälfte)
  n_ballslost_1 <- df_balls_lost %>% dplyr::filter(winx < own_area) %>% summarise(n())
  n_ballslost_2 <- df_balls_lost %>% filter(winx < own_half) %>% summarise(n())
  n_ballslost_3 <- nrow(df_balls_lost) - n_ballslost_2
  n_ballswon_1 <- df_balls_won %>% filter(winx < own_area) %>% summarise(n())
  n_ballswon_2 <- df_balls_won %>% filter(winx < own_half) %>% summarise(n())
  n_ballswon_3 <- nrow(df_balls_won) - n_ballswon_2
  vec_n_pass <- unlist(lapply(pass_areas, function(x) c(obtain_amount_passes(x, continued = FALSE),
                                                        obtain_amount_passes(x, continued = TRUE))
                              )
  )

  result <- c(n_pass, n_pass_1half, n_pass_2half, possession, n_pass_in_sequence,
              n_pass_s5_spw50, n_pass_l5_spw50, n_pass_l10_spw50,
              n_pass_area_all, n_pass_area,
              n_shots, n_shots_inside_area, n_shots_outside_area,
              n_shots_opp, n_shots_opp_inside_area, n_shots_opp_outside_area,
              n_ballslost_1, n_ballslost_2, n_ballswon_2, n_ballswon_3,
              vec_n_pass, ecken, ecken_gegner)

  myrownames <- c("Pässe", "Pässe 1.HZ", "Pässe 2.HZ", "Ballbesitz (in %)", "Durchschnitt Pässe in Passstafette",
                  "Passstafetten < 5 Pässe und >50m Raumgewinn", "Passstafetten >= 5 Pässe und >50m Raumgewinn", "Passstafetten >=10 Pässe und >50m Raumgewinn",
                  "Anteil angekommene Pässe ganzes Spielfeld",
                  "Anteil angekommene Pässe defensives Drittel", "Anteil angekommene Pässe mittleres Drittel", "Anteil angekommene Pässe offensives Drittel",
                 "Schüsse", "Schüsse innerhalb des Strafraums", "Schüsse außerhalb des Strafraums",
                 "Schüsse des Gegners", "Schüsse des Gegners innerhalb des Strafraums", "Schüsse des Gegners außerhalb des Strafraums",
                 "Ballverlust in und um eigenen Strafraum", "Ballverlust in eigener Hälfte",
                 "Ballgewinn in eigener Hälfte", "Ballgewinn in gegnerischer Hälfte",
                 "Öffnende Pässe von RV zu ZM", "Öffnende Pässe von RV zu ZM weitergeleitet",
                 "Pässe von RV zu RM long line", "Pässe von RV zu RM long line weitergeleitet",
                 "Öffnende Pässe von LV zu ZM", "Öffnende Pässe von LV zu ZM weitergeleitet",
                 "Pässe von LV zu LM long line", "Pässe von LV zu LM long line weitergeleitet",
                 "Lange Diagonalpässe von ZM auf RM", "Lange Diagonalpässe von ZM auf RM weitergeleitet",
                 "Lange Diagonalpässe von ZM auf LM", "Lange Diagonalpässe von ZM auf LM weitergeleitet",
                 "Ecken WFV", "Ecken Gegner")

  df_res <- unname(cbind(myrownames, result))
  colnames(df_res) <- c("Parameter", "Anzahl")
  return(df_res)
}
