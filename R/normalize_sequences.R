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
normalize_sequences <- function(df, dim_field = list(x = 105, y = 68)) {
  if (nrow(df) > 0) {
    df <- tidyr::drop_na(df)
    df_sel <- df %>%
      dplyr::select(V2, V3, V4)

    colnames(df_sel) <- c("Zeit", "x", "y")
    df_norm <- df_sel %>%
      dplyr::mutate(
        x = normalize_x(x, dim_field$x),
        y = normalize_y(y, dim_field$y)
      )
  } else {
    df_norm <- df
  }

  return(df_norm)
}
