#' Correct names from Tracktics export
#'
#' @param name string with input name
#'
#' @return string with correct name
#' @export
#'
#' @examples
tracktics_correct_names <- function(name) {
  check_pattern <- list(c("sler", "Dr"), "nschke", "Ku", "ffer", "thlein", "ndling")
  correct_names <- list("Drösler", "Hänschke", "Kuß", "Schäffer", "Röthlein", "Gündling")

  ret <- name

  for (i in 1:length(check_pattern)) {
    if (all(str_detect(name, check_pattern[[i]]))) ret <- correct_names[[i]]
  }

  return(ret)
}
