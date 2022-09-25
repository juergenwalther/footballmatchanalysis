#' Read stats of individual player
#'
#' @param file string with filename of whole team
#' @param player_first_name string for player first name
#' @param player_last_name string for player last name
#' @param match_names string for shortened match names of tracktics analysis
#'
#' @return vector with stats for one match of loaded file
#' @export
#'
#' @examples
tracktics_read_player_stats <- function(file, player_first_name, player_last_name = NA, match_name) {
  df <- read.csv(file = file)
  df_player <- df %>% filter(First.Name == player_first_name)
  if (!is.na(player_last_name)) {
    df_player <- df_player %>% filter(Last.Name == player_last_name)
  }

  # df_player <- df_player %>% dplyr::select(-c(
  #   Team, Date, Session.Type, Team.or.Player,
  #   Tracker, Inclusion,
  #   Speed..km.h.
  # ))

  if(nrow(df_player) == 0){
    ret = c()
    }else{

  ret <- c(
    match_name,
    df_player$Time.on.Pitch..mins.,
    round(as.numeric(df_player$Distance..km.)/as.numeric(df_player$Time.on.Pitch..mins.) * 1000,0),
    df_player$Distance..km., df_player$Distance.1st.Half..km., df_player$Distance.2nd.Half..km.,
    df_player$Distance.in.HI..km., df_player$Distance.in.HI.1st.Half.km., df_player$Distance.in.HI.2nd.Half..km.,
    round(c(df_player$Distance.in.HI..km. / df_player$Distance..km., df_player$Distance.in.HI.1st.Half.km. / df_player$Distance.1st.Half..km., df_player$Distance.in.HI.2nd.Half..km. / df_player$Distance.2nd.Half..km.) * 100, digits = 1),
    df_player$Sprints, df_player$Sprints.1st.Half, df_player$Sprints.2nd.Half,
    df_player$Progressive.Sprints, df_player$Progressive.Sprints.1st.Half, df_player$Progressive.Sprints.2nd.Half,
    round(c(df_player$Progressive.Sprints / df_player$Sprints, df_player$Progressive.Sprints.1st.Half / df_player$Sprints.1st.Half, df_player$Progressive.Sprints.2nd.Half / df_player$Sprints.2nd.Half) * 100, digits = 1),
    df_player$Top.Speed..km.h., df_player$Average.Sprint.Top.Speed
  )

  names(ret) <- c(
    "Match",
    "Gespielte Minuten",
    "Distanz pro gespielte Minute (in m)",
    "Distanz Spiel (in km)", "Distanz 1.HZ (in km)", "Distanz 2.HZ (in km)",
    "Distanz HI Spiel (in km)", "Distanz HI 1.HZ (in km)", "Distanz HI 2.HZ (in km)",
    "Anteil von HI an Gesamtdistanz (in %)", "Anteil von HI an Gesamtdistanz 1.HZ (in %)", "Anteil von HI an Gesamtdistanz 2.HZ (in %)",
    "Anzahl Sprints", "Anzahl Sprints 1.HZ", "Anzahl Sprints 2.HZ",
    "Anzahl progressiver Sprints", "Anzahl progressiver Sprints 1.HZ", "Anzahl progressiver Sprints 2.HZ",
    "Anteil progressiver Sprints an Sprints (in %)", "Anteil progressiver Sprints an Sprints 1.HZ (in %)", "Anteil progressiver Sprints an Sprints 2.HZ (in %)",
    "Sprint Höchstgeschwindigkeit (in km/h)", "Durchschnittliche Sprinthöchstgeschwindigkeit (in km/h)"
  )
  }

  return(ret)
}
