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
