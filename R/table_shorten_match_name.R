#' Convert long string of match names into short form
#'
#' @param match_names string. Each string vector element corresponds to a
#' match name in the form '20211120_WFV-Karlburg'
#'
#' @return string vector of shortened match names such as '2111_Karl'
#' @export
#'
#' @examples
table_shorten_match_name <- function(match_names) {
  ls_split_date <- strsplit(match_names, split = "_")
  ls_names <- lapply(ls_split_date, function(x) {
    month_year <- substring(x[1], 3, 6)
    match_split <- unlist(strsplit(x[2], split = "-"))
    red_opp_name <- substring(match_split[!(match_split == "WFV")], 1, 4)
    ret <- paste0(month_year, "_", red_opp_name)
    return(ret)
  })

  return(ls_names)
}
