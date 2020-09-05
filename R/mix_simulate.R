#' Make predictions on simulated data using mixed-effects model
#'
#' Data is simulated for each species (if multiple species are present) based on their respective ranges of the
#' predictor variable, which can be extrapolated to values defined by the user. The mixed-effects model is used
#' to predict values for the response variable, as well as it's prediction interval.
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
#' @param length.out Number of new X values to generate for each species. Defaults to 100. Set a higher value for greater resolution at the cost of computational time.
#' @param stat Specify whether the `"median"` or `"mean"` of simulated intervals are used.
#' @param n.sims Number of bootstrapped simulations to generate the prediction intervals. Defaults to `1000`.
#' @param response Column name of the response variable in `data`. Defaults to
#'   `height`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#' @param species Column name of the species variable in `data`. Defaults to `species`.
#' @param ... Additional arguments passed to [merTools::predictInterval()]
#'
#' @return Dataframe containing data generated from [generate_x()], and their predictions (with intervals).
#'
#' @seealso [mix_modelselect()], [generate_x()], [merTools::predictInterval()]
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

    # Error checking ------------------
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_choice(stat, choices = c("median", "mean"), add = coll)
    checkmate::assert_integerish(n.sims, lower = 1, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # generate new data using helper function
    newdat <- generate_x(data, extrapolate = extrapolate,
                         length.out = length.out,
                         response = response, predictor = predictor, species = species)

    # amendments for merTools::predictInterval
    names(newdat)[names(newdat) == 'predictor'] <- 'x'
    newdat <- as.data.frame(newdat)

    # Bootstrap prediction CI function
    # Species-level (group-level) predictions
    pred.boot <-
      merTools::predictInterval(
        modelselect$best_model,
        newdata = newdat,
        level = level,
        n.sims = n.sims,
        stat = stat,
        ...
      )

    # back-transform predictions for loglog and expo models
    best_mod_name <- rownames(modelselect$models_rank)[1]
    best_mod_formula <- strsplit(best_mod_name, "_")[[1]][1]
    if (best_mod_formula %in% c("loglog", "expo")) {
      pred.boot <- exp(pred.boot)
    }

    # merge newdat and predictions
    out <- cbind(newdat, pred.boot)
    names(out)[names(out) == 'x'] <- 'predictor' # change back colname

    return(out)

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
