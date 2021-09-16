#' Merge sequences of individual sections
#'
#' @param list_sequence list of sequences (passes, shots, shots of opponent)
#' @param n1 integer. index of \code{list_sequence} to begin merge
#' @param n2 integer. index of \code{list_sequence} to end merge
#'
#' @return data.frame with merged sequences
#' @export
#'
#' @examples
merge_sequences <- function(list_sequence, n1 = 1, n2 = length(list_sequence)) {
  df_tot <- do.call("rbind", lapply(n1:n2, function(i) {
    if (nrow(list_sequence[[i]] > 0)) {
      list_sequence[[i]] %>% dplyr::mutate(Zeit = Zeit + i * 100)
    } else {
      list_sequence[[i]]
    }
  }))
}
