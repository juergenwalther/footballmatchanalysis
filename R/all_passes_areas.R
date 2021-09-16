#' Title
#'
#' @param pass
#' @param areas
#' @param additonal_passes
#'
#' @return
#' @export
#'
#' @examples
all_passes_areas <- function(pass, areas, additional_passes = TRUE){
  # Passes of ouside defenders
  rvm <- do.call("rbind", lapply(1:(nrow(pass) - 1), get_passes_between_areas, pass, areas$rv, areas$m, additional_passes = additional_passes))
  rvrm <- do.call("rbind", lapply(1:(nrow(pass) - 1), get_passes_between_areas, pass, areas$rv, areas$rm, additional_passes = additional_passes))
  lvm <- do.call("rbind", lapply(1:(nrow(pass) - 1), get_passes_between_areas, pass, areas$lv, areas$m, additional_passes = additional_passes))
  lvlm <- do.call("rbind", lapply(1:(nrow(pass) - 1), get_passes_between_areas, pass, areas$lv, areas$lm, additional_passes = additional_passes))

  # Passes of midfield
  mrm <- do.call("rbind", lapply(1:(nrow(pass) - 1), get_passes_between_areas, pass, areas$m, areas$rm, additional_passes = additional_passes))
  mlm <- do.call("rbind", lapply(1:(nrow(pass) - 1), get_passes_between_areas, pass, areas$m, areas$lm, additional_passes = additional_passes))

  pass_areas <- list(rvm = rvm, rvrm = rvrm, lvm = lvm, lvlm = lvlm, mrm = mrm, mlm = mlm)
  return(pass_areas)
}
