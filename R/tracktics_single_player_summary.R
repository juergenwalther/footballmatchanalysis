#' Summary of Tracktics values of individual player
#'
#' @param data_single data.frame with Tracktics data of single player
#'
#' @return vector with mean of each row
#' @export
#'
#' @examples
tracktics_single_player_summary <- function(data_single) {
  mean_numeric <- function(x) {
    if (length(x[!is.na(x)]) > 0) {
      round(mean(as.numeric(x), na.rm = T), digits = 1)
    } else {
      NA
    }
  }

  sum_numeric <- function(x) {
    if (length(x[!is.na(x)]) > 0) {
      round(sum(as.numeric(x), na.rm = T), digits = 1)
    } else {
      NA
    }
  }

  if (is.null(dim(data_single))) {
    ret <- cbind(rep(NA, length(data_single)),rep(NA, length(data_single)))
  } else {
    sums <- apply(data_single, 1, sum_numeric)
    sums[c(2,9,10,11,18:22)] <- NA
    ret <- cbind(sums, apply(data_single, 1, mean_numeric))
  }
  return(ret)
}
