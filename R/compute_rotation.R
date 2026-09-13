#!/usr/bin/env Rscript


#' Apply Kabsch algorithm to find rotation to reference
#'
#' @export
#'
compute_rotation <- function(pointset, reference_pointset, prevent_mirroring = TRUE) {

  # standardize
  pts_interest_std <- standardize(pointset)
  pts_ref_std <- standardize(pointset)

  # (1) cross dispersion
  cross_dispersion <- as.matrix(t(pts_interest_std)) %*% as.matrix(pts_ref_std)

  # (2) singular value decomposition
  s <- svd(cross_dispersion, nu = 2)
  u <- s$u
  svals <- s$d
  v <- s$v

  # (3) prohibit mirroring
  if (prevent_mirroring && (det(u) * det(v) < 0.0)) {
    u[, 2] <- -u[, 2]
    svals[2] <- -1 * svals[2]
  }


  # optimal rotaiton
  rotation_matrix = u %*% v

  return(rotation_matrix)

} # /compute_rotation
