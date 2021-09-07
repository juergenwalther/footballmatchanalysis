#' Merge sequences of individual sections
#'
#' @param list_sequence list of sequences (passes, shots, shots of opponent)
#'
#' @return data.frame with merged sequences
#' @export
#'
#' @examples
merge_sequences <- function(list_sequence){
  df_tot <- do.call("rbind", lapply(1:length(list_sequence), function(i) {
    if(nrow(list_sequence[[i]] > 0)){
    list_sequence[[i]] %>% dplyr::mutate(Zeit = Zeit + i*100)
  } else {
    list_sequence[[i]]
  }
  }))
}
