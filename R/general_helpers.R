#!/usr/bin/env Rscript

### collection of minor helper functions ###


#' compose a rotation matrix from an angle (radians!)
#'
#' @export
#'
rotation_matrix_from_angle_rad <- \(rad) as.matrix(rbind(c(cos(rad), -sin(rad)), c(sin(rad), cos(rad))))

#' angles: degrees to radians
#'
#' @export
#'
deg2rad <- \(deg) deg*pi/180

#' angles: radians to degrees
#'
#' @export
#'
rad2deg <- \(rad) rad*180/pi


#' compose a rotation matrix from an angle (degrees!)
#'
#' @export
#'
rotation_matrix_from_angle_deg <- \(deg) rotation_matrix_from_angle_rad(deg2rad(deg))




## Inversions
# mostly trivial... for completeness and documentation.

#' invert a scaling factor
#'
#' @export
#'
invert_scaling <- \(s) 1/s

#' invert a translation
#'
#' @export
#'
invert_translation <- \(v) -v

#' invert a rotation matrix
#'
#' @export
#'
invert_rotation_matrix <- \(M) solve(M)

#' invert a shear operation
#'
#' @export
#'
invert_shear_matrix <- \(M) M * (diag(2) - 1 * as.matrix(!diag(2)))
