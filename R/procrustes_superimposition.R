#!/usr/bin/env Rscript


#' calculate Procrustes transformation parameters
#'
#' @export
#'
procrustes_superimposition <- function(
    points_interest,
    points_ref,
    disable_mirror = TRUE,
    mirror_x = FALSE
  ) {

  if (mirror_x) {
    points_interest[, 1] <- -1 * points_interest[, 1]
  }

  # store the rcs's and centroids
  rcs_interest <- compute_rcs(points_interest)
  rcs_ref      <- compute_rcs(points_ref)
  centroid_ref <- compute_centroid(points_ref)
  centroid     <- compute_centroid(points_interest)

  # standardize
  pts_interest_std <- standardize(points_interest)
  pts_ref_std      <- standardize(points_ref)

  # plot_points(points = pts_interest_std, ref_points = pts_ref_std)

  ### Kabsch algorithm to find the optimal rotation
  # (1) cross dispersion
  cross_dispersion <- as.matrix(t(pts_interest_std)) %*% as.matrix(pts_ref_std)

  # (2) singular value decomposition
  s <- svd(cross_dispersion, nu = 2)
  u <- s$u
  svals <- s$d
  v <- s$v

  # (3) prohibit mirroring
  if (disable_mirror && (det(u) * det(v) < 0.0)) {
    u[, 2] <- -u[, 2]
    svals[2] <- -1 * svals[2]
  }

  sval_trace <- sum(svals)

  # standarised distance
  procrustes_distance <- 1 - sval_trace^2

  # shift of centroid
  shift_vector <- centroid_ref - centroid

  # optimum scaling of data
  scaling_factor <- sval_trace * rcs_ref / rcs_interest

  # optimal rotaiton
  rotation_matrix <- u %*% v

  params <- list(
    "mirror_x" = mirror_x,
    "shift" = as.numeric(shift_vector),
    "centroid_ref" = as.numeric(centroid_ref),
    "scale" = scaling_factor,
    "rotation_matrix" = as.matrix(rotation_matrix),
    "procrustes_distance" = procrustes_distance
  )


  return(params)
} # /procrustes_superimposition



#' This helper function will apply a previously calculated series of transformations to data.
#'
#' Actions include, in order of appearance:
#'   - forced mirror x values on the y-axis
#'   - centering to the centroid
#'   - rotation by the rotation matrix
#'   - scaling with root centroid size
#'   - translation by a shift vector
#'
#' @export
#'
apply_transformations <- function(pointset, params) {

  v_shift <- params$shift
  mirror_x <- params$mirror_x
  f_scale <- params$scale
  rotation_matrix <- params$rotation_matrix

  # flip
  if (is.null(mirror_x)) mirror_x <- FALSE
  if (mirror_x) {
    pointset[, 1] <- -1 * pointset[, 1]
  }

  # rotate
  if (is.null(rotation_matrix)) {
    rotation_matrix <- diag(2)
  }
  pointset <- apply_rotation(pointset, rotation_matrix)

  # scale
  if (is.null(f_scale)) {
    f_scale <- 1.
  }
  pointset <- apply_scaling(pointset, f_scale)

  # shift
  if (is.null(v_shift)) {
    v_shift <- c(0., 0.)
  }

  pointset <- apply_translation(pointset, shift = v_shift)

  return(pointset)
} # /apply_transformations


#' this will do the opposite of what is stored in the transformations
#' (but possibly with a different point set)
#'
#' @export
#'
invert_transformations <- function(pointset, params) {

  shift <- invert_translation(params$shift)
  scale <- invert_scaling(params$scale)
  rotation_matrix <- invert_rotation_matrix(params$rotation_matrix)
  mirror_x <- params$mirror_x

  # un-shift
  if (is.null(shift)) {
    shift <- c(0., 0.)
  }
  pointset <- apply_translation(pointset, shift = shift)

  # un-scale
  if (is.null(scale)) {
    scale <- 1.
  }
  pointset <- apply_scaling(pointset, scale)

  # un-rotate
  if (is.null(rotation_matrix)) {
    rotation_matrix <- diag(2)
  }
  pointset <- apply_rotation(pointset, rotation_matrix)

  # un-chain my heart...
  if (is.null(mirror_x)) mirror_x <- FALSE
  if (mirror_x) {
    pointset[, 1] <- -1 * pointset[, 1]
  }

  return(pointset)
} # /invert_transformations
