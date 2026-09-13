#!/usr/bin/env Rscript


#' Transform points into Principal Component space
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param pca the result of PCA computation (list)
#'
#' @returns a transformed pointset
#'
#' @export
#'
apply_pca <- function(pointset, pca) {

  # center on the original centroid
  pointset <- apply_translation(
    pointset,
    shift = -1. * as.numeric(pca$centroid)
  )

  # transformation matrix (eigenvectors to rows for multiplication)
  pca_trafo_matrix <- t(pca$matrix)

  return(apply_matrix(
    pointset,
    transformation_matrix = pca_trafo_matrix
  ))
} # /apply_pca




#' Transform points back from Principal Component space
#'
#' @param pointset a n-by-m matrix of principal components
#' @param pca the result of PCA computation (list)
#'
#' @returns a pointset transformed back to the original space
#'
#' @export
#'
inverse_transform_pca <- function(pointset, pca) {

  # transformation matrix (eigenvectors in columns)
  pca_retrafo_matrix <- solve(t(pca$matrix))

  # inverse transformation
  pointset <- apply_matrix(
    pointset,
    transformation_matrix = pca_retrafo_matrix
  )

  # center on the original centroid
  pointset <- apply_translation(
    pointset,
    shift = as.numeric(pca$centroid)
  )

  # restore feature names
  colnames(pointset) <- pca$features


  return(pointset)
} # /inverse_transform_pca
