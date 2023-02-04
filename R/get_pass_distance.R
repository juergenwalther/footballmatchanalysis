#' Calculate distance of a pass
#'
#' @return
#' @export
#'
#' @examples
get_pass_distance <- function(pass){
  pass_distance <- pass %>% group_by(Zeit) %>%
    slice(c(2:pmax(2, n() - 1))) %>%
    ungroup() %>%
    mutate(dis = c(0,sqrt(diff(x)^2 + diff(y)^2)))

  return(pass_distance)
}
