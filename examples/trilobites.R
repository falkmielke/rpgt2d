#!/usr/bin/env Rscript

# testing features
# on known data sets

# REFERENCES
# Data examples according to Palaeomath101 - Norm MacLeod
# originally: https://www.palass.org/publications/newsletter/palaeomath-101
#             (accessed 20180928)
# Now available at:
#   https://paleonet.org/page-6/
#   https://paleonet.org//PaleoMath/05-02%20PCA.pdf
#   https://paleo-bio.com/wp-content/uploads/2026/05/2008-macleod.pdf

trilobite_data <- t(data.frame(
  "Acaste"         = c( 23.14 , 3.50  , 3.77  , 10.58),
  "Balizoma"       = c( 14.32 , 3.97  , 4.08  , 7.53 ),
  "Calymene"       = c( 51.69 , 10.91 , 10.72 , 18.19),
  "Ceraurus"       = c( 21.15 , 4.90  , 4.69  , 10.63),
  "Cheirurus"      = c( 31.74 , 9.33  , 12.11 , 13.99),
  "Cybantyx"       = c( 36.81 , 11.35 , 10.10 , 17.44),
  "Cybeloides"     = c( 25.13 , 6.39  , 6.81  , 12.49),
  "Dalmanites"     = c( 32.93 , 8.46  , 6.08  , 13.23),
  "Delphion"       = c( 21.81 , 6.92  , 9.01  , 10.62),
  "Ormathops"      = c( 13.88 , 5.03  , 4.34  , 6.46 ),
  "Phacopdina"     = c( 21.43 , 7.03  , 6.79  , 10.79),
  "Phacops"        = c( 27.23 , 5.30  , 8.19  , 11.03),
  "Placopoaria"    = c( 38.15 , 9.40  , 8.71  , 18.11),
  "Pricyclopyge"   = c( 40.11 , 14.98 , 12.98 , 14.12),
  "Ptychoparia"    = c( 62.17 , 12.25 , 8.71  , 24.25),
  "Rhenops"        = c( 55.94 , 19.00 , 13.10 , 23.15),
  "Sphaerexochus"  = c( 23.31 , 3.84  , 4.60  , 5.27 ),
  "Toxochasmops"   = c( 46.12 , 8.15  , 11.42 , 16.06),
  "Trimerus"       = c( 89.43 , 23.18 , 21.52 , 24.18),
  "Zacanthoides"   = c( 47.89 , 13.56 , 11.78 , 17.81)
))

colnames(trilobite_data) <- c(
  "body length (mm)",
  "glabellar length (mm)",
  "glabellar width (mm)",
  "eye spacing (mm)"
)

trilobite_data |> knitr::kable()



### testing PCA
pca <- compute_pca(as.matrix(trilobite_data))
# matches my prior calculations

transformed_data <- shift_to_point(as.matrix(trilobite_data)) %*% t(pca$matrix)
check_transformed <- apply_pca(as.matrix(trilobite_data), pca)

stopifnot(sum(abs(check_transformed - transformed_data)) < 1e-8)

check_retrafo <- inverse_transform_pca(as.matrix(check_transformed), pca)
check_retrafo |> knitr::kable()
