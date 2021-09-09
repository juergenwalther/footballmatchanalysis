get_passes_between_areas <- function(i, df, pos1, pos2){

  a = (df$Zeit[i] == df$Zeit[i+1]) & df$x[i] > pos1$x1 & df$x[i] < pos1$x2 & df$y[i] > pos1$y1 & df$y[i] < pos1$y2 & df$x[i+1] > pos2$x1 & df$x[i+1] < pos2$x2 & df$y[i+1] > pos2$y1 & df$y[i+1] < pos2$y2
  if(a){
    df$Zeit[c(i,i+1)] = i
    return(df[c(i,i+1),])
  }
  else return(NULL)


}
