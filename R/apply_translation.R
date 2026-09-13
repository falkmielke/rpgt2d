#!/usr/bin/env Rscript


#' shift points along a vector
#'
#' @export
#'
apply_translation <- function(pointset, shift) {
  pointset_shifted <- sweep(
      pointset,
      MARGIN = 2,
      STATS = as.numeric(shift),
      FUN = "+"
    )
  return(pointset_shifted)
} # /apply_translation


#' alias for `apply_translation`
#'
#' @export
#'
move <- apply_translation
