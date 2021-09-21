#' Calculate completed passes on data.frame of raw pass data
#'
#' @param pass data.frame with raw data of passes
#'
#' @return numeric value with amount of completed passes
#' @export
#'
#' @examples
get_all_passes_completed <- function(pass) {
  pass %>%
    group_by(Zeit) %>%
    summarise("total" = n() - 2) %>%
    select("total") %>%
    summarise_all(funs(sum))
}
