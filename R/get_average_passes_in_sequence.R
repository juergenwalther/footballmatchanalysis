#' Calculate number of average passes in a pass sequences
#'
#' @param pass data.frame with pass sequences
#'
#' @return data.frame with columns area (integer of area where ball was won),
#' space_won (amount of space won compared to total pitch length),
#' total (number of passes in pass sequence)
#' @export
#'
#' @examples
get_average_passes_in_sequence <- function(pass) {
  out <- pass %>%
    group_by(Zeit) %>%
    slice(-n()) %>%
    mutate(
      total = n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(total) %>%
    dplyr::summarise(across(everything(), mean))

  out <- round(as.numeric(out),1)

  return(out)
}
