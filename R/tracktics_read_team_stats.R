#' Extract parameters from Tracktics CSV file
#'
#' @param file string. full path to downloaded input csv from Tracktics "CSV Export"
#'
#' @return
#' @export
#'
#' @examples
tracktics_read_team_stats <- function(file) {
  df <- read.csv(file = file)
  total <- df[stringr::str_detect(df$Team.or.Player, "Team Total"), ]
  avg <- df[stringr::str_detect(df$Team.or.Player, "Team Average"), ]
  tot_dist <- total$Distance..km.

  ret <- round(
    c(
      total$Distance..km., total$Distance.1st.Half..km., total$Distance.2nd.Half..km.,
      avg$Distance..km., avg$Distance.1st.Half..km., avg$Distance.2nd.Half..km.,
      avg$Distance.in.HI..km. / avg$Distance..km. * 100, avg$Distance.in.HI.1st.Half.km. / avg$Distance.1st.Half..km. * 100, avg$Distance.in.HI.2nd.Half..km. / avg$Distance.2nd.Half..km. * 100,
      total$Sprints,
      avg$Sprints, avg$Sprints.1st.Half, avg$Sprints.2nd.Half,
      total$Progressive.Sprints, total$Progressive.Sprints.1st.Half, total$Progressive.Sprints.2nd.Half,
      #avg$Progressive.Sprints, avg$Progressive.Sprints.1st.Half, avg$Progressive.Sprints.2nd.Half,
      avg$Average.Sprint.Top.Speed
    ),
    digits = 1
  )

  names(ret) <- c("Distanz Spiel (in km)","Distanz 1.HZ (in km)","Distanz 2.HZ (in km)",
                  "Durschnittliche Distanz pro Spieler (in km)","Durschnittliche Distanz pro Spieler 1.HZ (in km)","Durschnittliche Distanz pro Spieler 2.HZ (in km)",
                  "Anteil von HI an Gesamtdistanz (in %)", "Anteil von HI an Gesamtdistanz 1.HZ (in %)", "Anteil von HI an Gesamtdistanz 2.HZ (in %)",
                  "Anzahl Sprints",
                  "Durchschnittliche Anzahl Sprints pro Spieler", "Durchschnittliche Anzahl Sprints pro Spieler 1.HZ", "Durchschnittliche Anzahl Sprints pro Spieler 2.HZ",
                  "Anzahl progressiver Sprints", "Anzahl progressiver Sprints 1.HZ", "Anzahl progressiver Sprints 2.HZ",
                  "Durchschnittliche Höchstgeschwindigkeit (in km/h)"
                  )
  return(ret)
}
