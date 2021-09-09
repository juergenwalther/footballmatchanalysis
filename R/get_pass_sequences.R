#' Erfassen aller Passstafetten länger als n Pässe
#'
#' Definiert als erster Punkt in Passstafetten
#'
#' @param pass_norm data.frame mit Passpositionen normiert auf das Spielfeld
#' @param n integer. Minimum length of sequence of passes to filter for
#'
#' @return data.frame with all balls gained
#' @export
#'
#' @examples
get_pass_sequences <- function(pass_norm, n) {
  df_pass_sequence = pass_norm %>% group_by(Zeit) %>% filter(length(x) > n)
  return(df_pass_sequence)
}
