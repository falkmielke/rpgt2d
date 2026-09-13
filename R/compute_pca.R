#!/usr/bin/env Rscript


#' Compute Principal Components (via Eigenanalysis) of a point matrix
#'
#' @param pointset a n-by-2 matrix of n 2D points
#'
#' @returns list with PCA results
#'
#' @export
#'
compute_pca <- function(pointset) {

  # eigenanalysis - stored in a list
  own <- list("features" = colnames(pointset),
    dim = ncol(pointset),
    n = nrow(pointset),
    raw_data = pointset
  )
  class(own) <- c("eigenanalysis")

  # store the original centroid
  own$centroid <- compute_centroid(pointset)

  # center the pointcloud
  pointset <- shift_to_point(pointset)

  # compute the covariance matrix
  # https://www.r-bloggers.com/2021/07/how-to-create-a-covariance-matrix-in-r
  covariance_matrix <- stats::cov(pointset)

  # eigenanalysis - SVD
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/svd
  s <- svd(covariance_matrix, nu = own$dim)
  own$values <- s$d
  own$matrix <- s$u

  own$vectors <- lapply(
    seq_len(ncol(own$matrix)),
    FUN = \(x) own$matrix[, x]
  )

  own$weights <- own$values / sum(own$values)

  return(own)

} # /compute_pca


# REFERENCES
# Data examples according to Palaeomath101
# https://www.palass.org/publications/newsletter/palaeomath-101
# (accessed 20180928)

# PCA testing done according to
# http://stats.stackexchange.com/questions/102882/steps-done-in-factor-analysis-compared-to-steps-done-in-pca/102999#102999
