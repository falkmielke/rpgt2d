#!/usr/bin/env Rscript

### collection of minor helper functions ###


#' compose a rotation matrix from an angle (radians!)
#'
#' @param rad angle in radians `[0, 2π]`
#'
#' @returns 2-by-2 rotation matrix
#'
#' @export
#'
rotation_matrix_from_angle_rad <- \(rad) as.matrix(rbind(c(cos(rad), -sin(rad)), c(sin(rad), cos(rad))))

#' angles: degrees to radians
#'
#' @param deg angle in degrees `[0, 360]`
#'
#' @returns angle in radians `[0, 2π]`
#'
#' @export
#'
deg2rad <- \(deg) deg*pi/180

#' angles: radians to degrees
#'
#' @param rad angle in radians `[0, 2π]`
#' @returns angle in degrees `[0, 360]`
#' @export
#'
rad2deg <- \(rad) rad*180/pi


#' compose a rotation matrix from an angle (degrees!)
#'
#' @param deg angle in degrees `[0, 360]`
#'
#' @returns 2-by-2 rotation matrix
#'
#' @export
#'
rotation_matrix_from_angle_deg <- \(deg) rotation_matrix_from_angle_rad(deg2rad(deg))




## Inversions
# mostly trivial... for completeness and documentation.

#' invert a scaling factor
#'
#' @param s scaling factor
#' @returns inverse scaling factor
#'
#' @export
#'
invert_scaling <- \(s) 1/s

#' invert a translation
#'
#' @param v translation vector
#' @returns inverse translation vector
#'
#' @export
#'
invert_translation <- \(v) -v

#' invert a rotation matrix
#'
#' @param M rotation matrix
#' @returns inverse rotation matrix
#'
#' @export
#'
invert_rotation_matrix <- \(M) solve(M)

#' invert a shear operation
#'
#' @param M shear matrix
#' @returns inverse shear matrix
#'
#' @export
#'
invert_shear_matrix <- \(M) M * (diag(2) - 1 * as.matrix(!diag(2)))
