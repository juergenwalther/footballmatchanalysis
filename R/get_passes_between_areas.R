get_passes_between_areas <- function(i, df, pos1, pos2, additional_passes = FALSE) {
  a <- (df$Zeit[i] == df$Zeit[i + 1]) & df$x[i] > pos1$x1 & df$x[i] < pos1$x2 & df$y[i] > pos1$y1 & df$y[i] < pos1$y2 & df$x[i + 1] > pos2$x1 & df$x[i + 1] < pos2$x2 & df$y[i + 1] > pos2$y1 & df$y[i + 1] < pos2$y2
  if (additional_passes == FALSE) {
    if (a) {
      df$Zeit[c(i, i + 1)] <- i
      return(df[c(i, i + 1), ])
    } else {
      return(NULL)
    }
  } else {
    if (a) {
      if (((i + 2) <= length(df$Zeit)) & (df$Zeit[i] == df$Zeit[i + 2])) {
        return(df[c(i, i + 1, i + 2), ])
      } else {
        # df$Zeit[c(i,i+1)] = i
        return(df[c(i, i + 1), ])
      }
    }
    else {
      return(NULL)
    }
  }
}
