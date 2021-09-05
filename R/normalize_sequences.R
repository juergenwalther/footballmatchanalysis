#' Normalize extracted sequences
#'
#' @param df data.frame with all extracted sequences
#' @param dim_field list with "X" and "y" element specifying the length of
#' pitch in x/y direction (usually set to 105/68)
#'
#' @return data.frame with df sequences normalized to pitch
#' @export
#'
#' @examples
normalize_sequences <- function(pass, dim_field) {
  df <- df %>% tidyr::drop_na

  df_sel <- df %>%
    select(V2, V3, V4)

  colnames(df_sel) <- c("Zeit", "x", "y")
  df_norm <- df_sel %>%
    mutate(
      x = normalize_x(x, dim_field$x),
      y = normalize_y(y, dim_field$y)
    )

  return(df_norm)
}
