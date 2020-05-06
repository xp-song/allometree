#' Street tree data in Singapore
#'
#' Example data on the height and trunk diameter of five tree species
#' planted along streets in Singapore (sample size = 1585).
#'
#' @docType data
#'
#' @usage data(urbantrees)
#'
#' @format A dataframe. Each row is a measurement for an individual tree.
#' The following variables are included as columns:
#'  * `species`: Name of tree species
#'  * `height` (m): Visually estimated height
#'  * `diameter` (m): Tree girth measured with diameter tape (at breast-height),
#'  and converted to diameter size
#'
#' @keywords datasets
#'
#' @examples
#' data(urbantrees)
#' unique(urbantrees$species) # species in the dataset
#'
#' table(urbantrees$species) # number of data points (rows) per species
"urbantrees"
