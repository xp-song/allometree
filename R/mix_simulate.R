#' Make predictions on simulated data using linear mixed-effects model
#'
#' Wrapper function that runs [mix_predict()] on simulated data from [generate_x()].
#' Data is generated for each species based on their respective ranges of the
#' predictor variable, which can be extrapolated to values defined by the user.
#' The mixed-effects model is used to predict values for the response variable, as well as it's prediction interval.
#' Necessary bias-corrections are made if the mixed-effects model has a transformed response variable.
#'
#' @param data Dataframe used to generate data and their predictions the using mixed-effects model.
#' Columns should contain the species and variables of interest. Each row is a measurement for an individual tree.
#' @param modelselect Output from the [mix_modelselect()] function.
#' @param level Level of confidence for the prediction interval. Defaults to
#'  `0.95`.
#' @param extrapolate Numeric vector of 2 elements (e.g. `c(0,4)`), representing
#'  the upper and lower bounds of extrapolation. Defaults to `NULL` for no
#'  extrapolation.
#' @param length.out Number of new predictor values to generate for each species. Defaults to 100.
#' Set a higher value for greater resolution at the cost of computational time.
#' @param stat Specify whether the `"median"` or `"mean"` of simulated intervals are used.
#' @param n.sims Number of bootstrapped simulations to generate the prediction intervals. Defaults to `1000`.
#' @param response Column name of the response variable in `data`. Defaults to
#'   `height`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#' @param species Column name of the species variable in `data`. Defaults to `species`.
#' @param ... Additional arguments passed to [merTools::predictInterval()]
#'
#'@return A dataframe with columns: \describe{
#'  \item{species}{Name of tree species.}
#'  \item{predictor}{Variable used to make predictions.}
#'  \item{fit}{Predicted value.}
#'  \item{lwr}{Lower bound of the prediction interval, based on the input argument `level`.}
#'  \item{upr}{Upper bound of the prediction interval, based on the input argument `level`.}
#'  \item{extrapolated}{Indicates whether the predictions are based on
#'  extrapolated values. Either 'High', 'Low', or 'No' (not extrapolated).} }
#'
#' @family mixed-effects model functions
#' @seealso [generate_x()] to generate new values for each species in a dataset.
#'
#'   [mix_predict()] to make predictions for all species in a dataset using linear mixed-effects model.
#'
#'   [merTools::predictInterval()] to make predictions from models fit with the `lme4` package.
#'
#' @examples
#' data(urbantrees)
#'
#' \dontrun{
#' model <- mix_modelselect(urbantrees)
#' results <- mix_simulate(data = urbantrees, modelselect = model)
#' head(results)
#' }
#'
#'@import checkmate
#'@importFrom merTools predictInterval
#'
#' @export
mix_simulate <-
  function(data, modelselect,
           level = 0.95,
           extrapolate = NULL, length.out = 100,
           stat = "median", n.sims = 1000,
           response = "height", predictor = "diameter", species = "species", ...) {


    # Calculations ------------------

    # generate new data using helper function
    newdat <- generate_x(data,
                         extrapolate = extrapolate,
                         length.out = length.out,
                         response = response, predictor = predictor,
                         species = species)

    results <- mix_predict(data = newdat,
                           modelselect = modelselect,
                           level = level,
                           stat = stat, n.sims = n.sims,
                           predictor = "predictor", species = "species", ...) # define colnames based on newdat output

    return(results)

  }



# ## Population-level predictions (no species random effect a.k.a. predict on new species)
# avg_sp <- as.character(averageObs(mix_modlme4)$species) # perform population-lvl bootstrap using the "average" species
# mix_newdatapop$species <- avg_sp
# pred.boot.pop <-
#   predictInterval(mix_modlme4,
#                   newdata = mix_newdatapop,
#                   level = 0.95,
#                   stat = "median",
#                   returnSims = TRUE,
#                   seed = 123) %>%
#   mutate_all(~exp(.) * mix_modlmer_cf)
