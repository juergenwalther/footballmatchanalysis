#' Obtain match name from file path
#'
#' @param file_path specify file path with saved team Tracktics csv
#'
#' @return string containing name of match
#' @export
#'
#' @examples
obtain_match_names <- function(file_path){
  spiele <- list.files(path = paste0(file_path), pattern = "*csv", full.names = FALSE)
  spiele <- substring(spiele, 1, nchar(spiele)-4)
  ret <- spiele[-length(spiele)]

  return(ret)
}
