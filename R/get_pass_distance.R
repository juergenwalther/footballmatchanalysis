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
    mutate() %>%

}


euclidean_dist <- function(x, y) sqrt(sum((x - y)^2))
x = pass$x[1]

sqrt(sum((x - y)^2))
