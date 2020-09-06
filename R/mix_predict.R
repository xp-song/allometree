#' Make predictions for all species in a dataset using linear mixed-effects model
#'
#' Make predictions across rows in a dataset that may contain multiple species.
#' The mixed-effects model is used to predict values for the response variable,
#' as well as it's prediction interval. Necessary bias-corrections are made if
#' the mixed-effects model has a transformed response variable.
#'
#' @param data Dataframe with columns containing the species and variables of
#'   interest. Each row is a measurement for an individual tree.
#' @param modelselect Output from the [mix_modelselect()] function.
#' @param level Level of confidence for the prediction interval. Defaults to
#'  `0.95`.
#' @param stat Specify whether the `"median"` or `"mean"` of simulated intervals are used.
#' @param n.sims Number of bootstrapped simulations to generate the prediction intervals. Defaults to `1000`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#' @param species Column name of the species variable in `data`. Defaults to `species`.
#' @param ... Additional arguments passed to [merTools::predictInterval()]
#'
#' @return Dataframe of input `data` with columns appended: \describe{
#'   \item{fit}{Predicted value for the response variable.}
#'   \item{lwr}{Lower bound of the prediction interval, based on the input argument `level`.}
#'   \item{upr}{Upper bound of the prediction interval, based on the input argument `level`.} }
#'
#' @family mixed-effects model functions
#' @seealso [merTools::predictInterval()] to make predictions from models fit with the `lme4` package.
#'
#' @examples
#' data(urbantrees)
#'
#' \dontrun{
#' model <- mix_modelselect(urbantrees)
#'
#' Alb_sam <- urbantrees[urbantrees$species == 'Albizia saman', ]  # use one species as an example
#' results <- mix_predict(data = Alb_sam, modelselect = model,
#'                        predictor = "diameter") # make predictions for measured values
#'
#' head(results)
#' }
#'
#'@import checkmate
#'@importFrom merTools predictInterval
#'
#' @export
mix_predict <-
  function(data, modelselect,
           level = 0.95,
           stat = "median", n.sims = 1000,
           predictor = "diameter", species = "species", ...) {

    # Error checking ------------------
    coll <- checkmate::makeAssertCollection()

    checkmate::assert_choice(stat, choices = c("median", "mean"), add = coll)
    checkmate::assert_integerish(n.sims, lower = 1, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    newdat <- data

    # amend var names to match what was used to fit model
    names(newdat)[names(newdat) == predictor] <- 'x'
    names(newdat)[names(newdat) == species] <- 'species'

    # merTools::predictInterval needs df input
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
    names(out)[names(out) == 'x'] <- predictor # change back colname
    names(out)[names(out) == 'species'] <- species # change back colname

    return(out)

  }
