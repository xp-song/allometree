#' Heights and diameter sizes of 1,585 street trees in Singapore
#'
#' A sample dataset containing the height and trunk diameter of five tree species,
#' planted along streets in Singapore. Each row is a measurement for an individual tree.
#'
#' @docType data
#'
#' @format A dataframe with 1585 rows and 3 variables:
#' \describe{
#'   \item{species}{Name of tree species.}
#'   \item{height}{Tree height based on visual estimation by an arborist (m).}
#'   \item{diameter}{Tree girth measured with diameter tape (at breast-height),
#'  and converted to diameter size (m).}
#'  }
#'
#' @keywords datasets
#'
#' @source
#' Independently sampled validation data reported in:
#'
#' Song, X. P., Lai, H. R., Wijedasa, L. S., Yee, A. T. K., Tan, P. Y., Richards, D. R.,
#' Streamlining management practices based on the size allometry of tropical street trees (in prep).
#'
#' @examples
#' data(urbantrees)
#' unique(urbantrees$species) # species in the dataset
#'
#' table(urbantrees$species) # number of data points (rows) per species
"urbantrees"
