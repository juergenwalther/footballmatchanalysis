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
  return(round(
    c(
      total$Distance..km., total$Distance.1st.Half..km., total$Distance.2nd.Half..km.,
      avg$Distance..km., avg$Distance.1st.Half..km., avg$Distance.2nd.Half..km.,
      total$Sprints,
      avg$Sprints, avg$Sprints.1st.Half, avg$Sprints.2nd.Half,
      total$Progressive.Sprints, total$Progressive.Sprints.1st.Half, total$Progressive.Sprints.2nd.Half,
      avg$Progressive.Sprints, avg$Progressive.Sprints.1st.Half, avg$Progressive.Sprints.2nd.Half,
      avg$Distance.in.HI..km. / avg$Distance..km., avg$Distance.in.HI.1st.Half.km. / avg$Distance.1st.Half..km., avg$Distance.in.HI.2nd.Half..km. / avg$Distance.2nd.Half..km.,
      avg$Average.Sprint.Top.Speed
    ),
    digits = 2
  ))
}
