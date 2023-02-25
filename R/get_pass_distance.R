#' Calculate distance of passes
#'
#' @param pass data.frame mit Passpositionen normiert auf das Spielfeld
#'
#' @return data.frame with additional column dis for distance of passes
#' @export
#'
#' @examples
get_pass_distance <- function(pass){
  pass_distance <- pass %>% group_by(Zeit) %>%
    slice(c(1:pmax(1, n() - 1))) %>%
    mutate(dis = c(0,sqrt(diff(x)^2 + diff(y)^2))) %>%
    filter(dis > 0) %>%
    ungroup()

  return(pass_distance)
}
