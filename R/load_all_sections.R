#' Load all csv files of a path analysis via GIMP
#'
#' @param filename character string. Filename is either "paesse", "schuesse" or "schuesse_gegner"
#' @param spiel charcater string. Folder name of match where video analysis is saved
#' @param path character string. Parent path of spiel folder
#' @param num_sections numeric. Integer value on the number of sections
#'
#' @return list of data.frames with content of video analysis.
#' Length of list corresponds to \code{num_sections}
#' @export
#'
#' @examples
load_all_sections <- function(filename, spiel, path, num_sections = 6) {
  out_list <- lapply(1:num_sections, footballmatchanalysis:::load_csv_data(filename, spiel, path))
  return(out_list)
}
