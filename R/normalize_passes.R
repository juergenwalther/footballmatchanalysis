#' Normalize passes
#'
#' @param pass data.frame with all pass sequences
#' @param dim_field list with "X" and "y" element specifying the length of
#' pitch in x/y direction (usually set to 105/68)
#'
#' @return data.frame with pass sequences normalized to pitch
#' @export
#'
#' @examples
normalize_passes <- function(pass, dim_field){
  pass = pass %>% tidyr::drop_na

  pass_sel = pass %>%
    select(V2,V3,V4)

  colnames(pass_sel) = c("Zeit","x","y")
  pass_norm = pass_sel %>%
    mutate(x = normalize_x(x,dim_field$x),
           y = normalize_y(y,dim_field$y))

  return(pass_norm)
}
