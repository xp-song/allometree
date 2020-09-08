#' Generate new predictor values for each species in a dataset
#'
#' Helper function to generate a sequence of new values for each species,
#' based on the range of values for the predictor variable in the `data`.
#' Upward and downward extrapolation to specified values can be made.
#' Predictions can subsequently be made on the generated values.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree of a particular species.
#' @param response Column name of the response variable in `data`. Defaults to
#'   `height`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#' @param species Column name of the species variable in `data`. Defaults to `species`.
#' @param extrapolate Numeric vector of 2 elements (e.g. `c(0,4)`), representing
#'  the upper and lower bounds of extrapolation. Defaults to `NULL` for no
#'  extrapolation.
#' @param length.out Number of new values to generate for each species. Defaults to 100. Set a higher value for greater resolution at the cost of computational time.
#'
#' @return A dataframe with columns:
#' \describe{
#'  \item{species}{Name of tree species.}
#'  \item{predictor}{Newly generated predictor values.}
#'  \item{extrapolated}{Indicates whether the predictor (x) values are extrapolated beyond the supplied dataset.
#'   Either 'High', 'Low', or 'No' (not extrapolated).}
#'  }
#'
#' @examples
#  data(urbantrees)
#' newdata <- generate_x(urbantrees,
#'                       extrapolate = c(0,4),
#'                       response = "height", predictor = "diameter")
#' head(newdata)
#'
#' @import checkmate
#' @importFrom stats complete.cases
#' @importFrom tidyr pivot_longer
#'
#' @export
generate_x <-
  function(data,
           response = "height",
           predictor = "diameter",
           species = "species",
           extrapolate = NULL,
           length.out = 100) {

    # Error checking ------------------

    if (is.null(data[[response]])) {
      stop("Variable name assigned to 'response' not found in 'data'.")
    }
    if (is.null(data[[predictor]])) {
      stop("Variable name assigned to 'predictor' not found in 'data'.")
    }
    if (is.null(data[[species]])) {
      stop("Variable name assigned to 'species' not found in 'data'.")
    }

    coll <- checkmate::makeAssertCollection()
    checkmate::assert_data_frame(data, add = coll)
    checkmate::assert_subset(response, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(predictor, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(species, choices = colnames(data), empty.ok = FALSE, add = coll)

    checkmate::assert_numeric(data[[response]], lower = 1e-05, finite = TRUE, .var.name = "response", add = coll)
    checkmate::assert_numeric(data[[predictor]], lower = 1e-05, finite = TRUE, .var.name = "predictor", add = coll)
    checkmate::assert(checkmate::check_character(data[[species]]), checkmate::check_factor(data[[species]]), combine = "or", .var.name = "species")

    checkmate::assert_integerish(length.out, lower = 1, add = coll)

    checkmate::reportAssertions(coll)

    # remove missing data
    if (checkmate::anyMissing(data[[response]]) | checkmate::anyMissing(data[[predictor]])) {
      message(cat(sum(!complete.cases(data[, c(response, predictor)])), " row(s) with missing value(s) removed from 'data'", sep = ""))
      data <- data[complete.cases(data[, c(response, predictor)]), ]
    }


    # Calculations ------------------

    y <- data[[response]]
    x <- data[[predictor]]
    sp <- data[[species]]
    y_trans <- log(y)

    # x range of each species
    x_sp_range <- as.data.frame(do.call(rbind, tapply(x, sp, range)))
    names(x_sp_range) <- c("x_min", "x_max")
    x_sp_range$sp <- rownames(x_sp_range)

    # generate new data across x range
    newdata <- apply(x_sp_range, 1, function(x) seq(x["x_min"], x["x_max"], length.out = 100))  #each sp is a column
    newdata <- tidyr::pivot_longer(as.data.frame(newdata), cols = colnames(newdata), names_to = "species", values_to = "predictor")

    newdata$extrapolated <- "No"


    # extrapolated x values
    if (!is.null(extrapolate)) {
      checkmate::assert_numeric(extrapolate, len = 2, lower = 0, sorted = TRUE, any.missing = FALSE)

      # extrapolate[1] to x_min
      newdata_low <- apply(x_sp_range, 1, function(x) if (x["x_min"] > extrapolate[1]) {
        seq(extrapolate[1], x["x_min"], length.out = length.out)
        } else {
          rep(NA, length.out)
          })  #each sp is a column
      if (!is.null(newdata_low)) {
        newdata_low <- as.data.frame(newdata_low)
        colnames(newdata_low) <- x_sp_range$sp

        newdata_low <- tidyr::pivot_longer(newdata_low, cols = colnames(newdata_low), names_to = "species", values_to = "predictor")
        newdata_low$extrapolated <- "Low"
      }

      # extrapolate[2] to x_max
      newdata_high <- apply(x_sp_range, 1, function(x) if (x["x_max"] < extrapolate[2]) {
        seq(extrapolate[2], x["x_max"], length.out = length.out)
        } else {
          rep(NA, length.out)
          })  #each sp is a column

      if (!is.null(newdata_high)) {
        newdata_high <- as.data.frame(newdata_high)
        colnames(newdata_high) <- x_sp_range$sp
        newdata_high <- tidyr::pivot_longer(newdata_high, cols = colnames(newdata_high), names_to = "species", values_to = "predictor")
        newdata_high$extrapolated <- "High"
      }

      newdata_extra <- rbind.data.frame(newdata_low, newdata_high)
      newdata_extra <- unique(newdata_extra)  # remove duplicated rows
      newdata_extra <- newdata_extra[complete.cases(newdata_extra), ]

      # combine with non-extrapolated data
      newdata <- rbind.data.frame(newdata, newdata_extra)
    }

    newdata <- newdata[order(newdata$species, newdata$predictor), ]


    return(newdata)
}


