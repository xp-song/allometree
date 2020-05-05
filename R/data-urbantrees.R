#' Sample of tree heights and diameter sizes
#'
#' Example data on the size dimensions of five urban tree species
#' planted along streets in the tropical city of Singapore.
#'
#' @docType data
#'
#' @usage data(urbantrees)
#'
#' @format A dataframe with columns containing the \code{'species'},
#' \code{'height'} (m) and \code{'diameter'} (m).
#' Each row is a measurement for an individual tree (no tree is represented more than once).
#' There are 100 measurements per species, across a broad range of height and diameter sizes.
#'
#' @keywords datasets
#'
#' @examples
#' data(urbantrees)
#' unique(urbantrees$species) # species in the dataset
#'
#' table(urbantrees$species) # number of data points (rows) per species
"urbantrees"
