get_won_space_pass_sequence <- function(pass) {
  pass %>%
    group_by(Zeit) %>%
    slice(-n()) %>%
    mutate(
      space_won = (last(x) - first(x))/105*100,
      total = n()
    ) %>%
    dplyr::select(Zeit, space_won, total) %>%
    filter(space_won != 0) %>%
    slice(1)
}
