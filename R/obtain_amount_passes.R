obtain_amount_passes <- function(df, continued = TRUE) {
  if (continued == TRUE) {
    res <- df %>%
      dplyr::group_by(Zeit) %>%
      dplyr::filter(n() > 2) %>%
      dplyr::ungroup() %>%
      dplyr::select(Zeit) %>%
      dplyr::distinct() %>%
      summarise(n())
  } else {
    res <- df %>%
      dplyr::select(Zeit) %>%
      dplyr::distinct() %>%
      summarise(n())
  }
  return(res)
}
