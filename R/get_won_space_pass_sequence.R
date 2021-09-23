#' Calculate won space for all pass sequences
#'
#' @param pass data.frame with pass sequences
#' @param area1 integer. Set to 35 (defensive third)
#' @param area2 integer. Set to 70 (middle third)
#' @param area3 integer. Set to 105 (offensive third)
#'
#' @return data.frame with columns area (integer of area where ball was won),
#' space_won (amount of space won compared to total pitch length),
#' total (number of passes in pass sequence)
#' @export
#'
#' @examples
get_won_space_pass_sequence <- function(pass, area1 = 35, area2 = 70, area3 = 105) {
  pass %>%
    mutate(area = ifelse(x < area1, 1, ifelse(x < area2, 2, 3))) %>%
    group_by(Zeit) %>%
    slice(-n()) %>%
    mutate(
      space_won = (last(x) - first(x))/105*100,
      total = n()
    ) %>%
    dplyr::select(Zeit, area, space_won, total) %>%
    filter(space_won != 0) %>%
    slice(1)
}
