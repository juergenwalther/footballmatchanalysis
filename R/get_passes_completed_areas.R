#' Get completed and total passes in different areas of pitch
#'
#' @param pass_tot
#' @param area1 integer. Set to 35 (defensive third)
#' @param area2 integer. Set to 70 (middle third)
#' @param area3 integer. Set to 105 (offensive third)
#'
#' @return data.frame with completed, total passes in all threeareas
#'  as well as percentage of completion for each area (perc) and total (all_perc)
#' @export
#'
#' @examples
get_passes_completed_areas <- function(pass_tot, area1 = 35, area2 = 70, area3 = 105) {

  pass_areas_completed <- pass_tot %>%
    mutate(area = ifelse(x < area1, 1, ifelse(x < area2, 2, 3))) %>%
    group_by(Zeit) %>%
    slice(c(2:pmax(2, n() - 1))) %>%
    ungroup() %>%
    add_count(area) %>%
    rename(completed = n) %>%
    group_by(area) %>%
    slice(1) %>%
    ungroup() %>%
    select(area, completed)

  pass_areas_total <- pass_tot %>%
    mutate(area = ifelse(x < area1, 1, ifelse(x < area2, 2, 3))) %>%
    group_by(Zeit) %>%
    slice(c(2:n())) %>%
    ungroup() %>%
    add_count(area) %>%
    rename(total = n) %>%
    group_by(area) %>%
    slice(1) %>%
    ungroup() %>%
    select(area, total)

  out <- inner_join(pass_areas_completed, pass_areas_total)
  out <- out %>% mutate(perc = completed/total,
                        all_perc = sum(completed)/sum(total))

  return(out)
}
