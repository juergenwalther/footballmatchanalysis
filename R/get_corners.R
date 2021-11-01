#' Get corner kicks from Excel file
#'
#' @param rownum_team integer. row number in Excel file with corner kicks for your team
#' @param colnum_team integer. column number in Excel file with corner kicks for your team
#' @param rownum_opp integer. row number in Excel file with corner kicks for opponent team
#' @param colnum_opp integer. column number in Excel file with corner kicks for opponent team
#' @param file character. file path to Excel file where number of corner kicks are stored
#'
#' @return numeric. 1st element: corner kicks your team. 2nd element: corner kicks opponent team
#' @export
#'
#' @examples
get_corners <- function(file, rownum_team = 4, colnum_team = 9,
                        rownum_opp = 4, colnum_opp = 10) {
  data <- readxl::read_xlsx(file)
  return(c(
    as.numeric(data[rownum_team - 1, colnum_team]),
    as.numeric(data[rownum_opp - 1, colnum_opp])
  ))
}
