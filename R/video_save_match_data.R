video_save_match_data <- function(spiel, spielrichtung_real,
                                  pass_tot,
                                  shots_tot,
                                  shots_opp_tot,
                                  pass_areas_tot,
                                  pass_space_won_tot,
                                  ecken,
                                  ecken_gegner,
                                  summary_table,
                                  filepath){

  save(spiel,
       spielrichtung_real,
       pass_tot,
       shots_tot,
       shots_opp_tot,
       pass_areas_tot,
       pass_space_won_tot,
       ecken,
       ecken_gegner,
       summary_table, file = filepath)
}
