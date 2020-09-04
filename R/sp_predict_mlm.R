#' Predictions or simulation for mixed-effects or multilevel models
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree.
#' @param models Output from the [allometree_mlm()] function.
#' @param level The width of the prediction intervals
#' @param stat Take the median or mean of simulated intervals
#' @param n.sims Number of bootstrap simulation for the prediction intervals
#' @param ... Additional arguments passed to [merTools::predictInterval()]
#' @return A dataframe containing generated newX from [generate_newX()] and predictions (with intervals)
#' @seealso [allometree_mlm()], [generate_newX()], [merTools::predictInterval()]
#' @examples
#' dat <- subset(urbantrees, height < 40)
#' m_test <- allometree_mlm(dat)
#' pred_mlm <- sp_predict_mlm(dat, m_test)
#' pred_mlm


sp_predict_mlm <-
  function(data, models, level = 0.95, stat = "median", n.sims = 1000, ...) {

    # generate new data using helper function
    newdat <- generate_newX(data)

    # Bootstrap prediction CI function
    # Species-level (group-level) predictions
    pred.boot <-
      merTools::predictInterval(
        models$best_model,
        newdata = newdat,
        level = level,
        n.sims = n.sims,
        stat = stat,
        ...
      )

    # back-transform predictions for loglog and expo models
    best_mod_name <- rownames(models$models_rank)[1]
    best_mod_formula <- strsplit(best_mod_name, "_")[[1]][1]
    if (best_mod_formula %in% c("loglog", "expo")) {
      pred.boot <- exp(pred.boot)
    }

    # merge newdat and predictions
    out <- cbind(newdat, pred.boot)

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
