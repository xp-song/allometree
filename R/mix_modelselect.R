#' Fit linear mixed-effects model across all species
#'
#' Fit data to linear mixed-effects model with 'species' specified as the random effect,
#' using the `lme4::lmer` function under the hood. The full list of allometric equations that are
#' considered can be found in `?eqns_info` and `data(eqns_info)`.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree.
#' @param species Column name of the species variable in `data`. Defaults to `species`.
#' @param response Column name of the response variable.
#'   Defaults to `height`.
#' @param predictor Column name of the predictor variable.
#'   Defaults to `diameter`.
#' @return A list of 5 elements:
#'   \describe{
#'     \item{models_rank}{A model selection table of all the types of mixed-effects models considered,
#'     ranked in order of ascending Aikake's Information Criterion corrected for small sample sizes (AICc).
#'     Model details can be found in `?eqns_info` and `data(eqns_info)`.}
#'     \item{best_model}{The best-fit model object.}
#'     \item{R2}{The conditional and marginal pseudo-\eqn{R^2} of the best-fit model.}
#'     \item{CF}{Correction factor used to adjust predicted values if response variable is transformed
#'     (incorporated into reported parameters).}
#'     \item{warnings}{Warning messages, if any, spit from the models.
#'       These usually indicate failure of model convergence.}
#'     }
#'
#' @family mixed-effects model functions
#' @examples
#' data(urbantrees)
#'
#' \dontrun{
#' mix_modelselect(data = urbantrees,
#'                 species = "species",
#'                 response = "height", predictor = "diameter")
#' }
#'
#' @import checkmate
#' @import lme4
#' @importFrom MuMIn model.sel r.squaredGLMM
#' @importFrom stats coef complete.cases
#' @importFrom sjstats rmse
#' @export
mix_modelselect <-
  function(data,
           species = "species",
           response = "height",
           predictor = "diameter") {

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

    # positive numeric variables
    coll <- checkmate::makeAssertCollection()
    checkmate::assert_data_frame(data, add = coll)
    checkmate::assert_subset(response, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(predictor, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(species, choices = colnames(data), empty.ok = FALSE, add = coll)

    checkmate::assert_numeric(data[[response]], lower = 1e-05, finite = TRUE, .var.name = "response", add = coll)
    checkmate::assert_numeric(data[[predictor]], lower = 1e-05, finite = TRUE, .var.name = "predictor", add = coll)
    checkmate::assert(checkmate::check_character(data[[species]]), checkmate::check_factor(data[[species]]), combine = "or", .var.name = "species")

    checkmate::reportAssertions(coll)

    # remove missing data
    if (checkmate::anyMissing(data[[response]]) | checkmate::anyMissing(data[[predictor]])) {
      message(cat(sum(!complete.cases(data[, c(response, predictor)])), " row(s) with missing value(s) removed from 'data'", sep = ""))
      data <- data[complete.cases(data[, c(response, predictor)]), ]
    }


    # Calculations ------------------

    # Calculate geometric mean height - necessary for comparing AICc of transformed models
    geom_mean_y <- exp(mean(log(data[[response]])))

    # extract values to use in model
    y <- data[[response]]
    x <- data[[predictor]]
    y_trans <- log(y)
    y_trans_adj <- y_trans * geom_mean_y
    species <- data[[species]]

    # formula list
    # for model selection
    mix.formula.list <- list(
      lin = as.formula("y ~ 1 + x + (1 + x | species)"),
      quad = as.formula("y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | species)"),
      cub = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + (1 + x + I(x^2) + I(x^3) | species)"),
      quart = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + (1 + x + I(x^2) + I(x^3) + I(x^4) | species)"),
      loglog = as.formula("y_trans_adj ~ 1 + I(log(log(x+1))) + (1 + I(log(log(x+1))) | species)"),
      expo = as.formula("y_trans_adj ~ 1 + x + (1 + x | species)")
    )
    # for refitting the best model (not using adjusted response var for transformed models)
    refit.formula.list <- list(
      lin = as.formula("y ~ 1 + x + (1 + x | species)"),
      quad = as.formula("y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | species)"),
      cub = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + (1 + x + I(x^2) + I(x^3) | species)"),
      quart = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + (1 + x + I(x^2) + I(x^3) + I(x^4) | species)"),
      loglog = as.formula("y_trans ~ 1 + I(log(log(x+1))) + (1 + I(log(log(x+1))) | species)"),
      expo = as.formula("y_trans ~ 1 + x + (1 + x | species)")
    )

    # weights
    weight.list <- list(
      w1 = NULL,
      w2 = sqrt(x),
      w3 = x,
      w4 = x^2
    )

    # fit mixed models in a loop
    # consider refarctoring this to lapply or parallelisation in the future
    mix.list <- list()
    i <- 1
    for (f in seq_len(length(mix.formula.list))) {
      for (w in seq_len(length(weight.list))) {
        message(paste0("Fitting ", names(mix.formula.list)[f], " model with weight ", names(weight.list)[w]))
        mix.list[[i]] <-
          lme4::lmer(
            formula = mix.formula.list[[f]],
            weights = weight.list[[w]],
            control = lme4::lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 10000)),
            REML = FALSE
          )
        i <- i + 1
      }
    }
    names(mix.list) <-
      as.vector(t(outer(names(mix.formula.list), names(weight.list), paste, sep = "_")))

    # print convergence and warning messages
    mix_msg <- lapply(mix.list, function(m) m@optinfo$conv$lme4$messages)
    mix_msg <- mix_msg[!sapply(mix_msg, is.null)]  # remove NULLs

    if (length(mix_msg) > 0) {
      message("Oops, some models have warning messages. See 'warnings' in the output list.")
    }

    # Model selection
    mix_comp <- MuMIn::model.sel(mix.list)
    mix_comp_cols_print <- c("df", "logLik", "AICc", "delta")

    # Best model
    best_mod_name <- rownames(mix_comp)[1]
    best_mod_formula <- strsplit(best_mod_name, "_")[[1]][1]
    best_mod_weight <- strsplit(best_mod_name, "_")[[1]][2]
    # mix_best <- get.models(mix_comp, subset = 1)[[1]]
    # refit the best model using REML, and
    # change the response if it is a loglog or expo model
    best_mod_refit <-
      lme4::lmer(
        formula = refit.formula.list[[best_mod_formula]],
        weights = weight.list[[best_mod_weight]],
        control = lme4::lmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 10000)),
        REML = TRUE
      )

    # Species-specific parameters and information
    # Parameters
    params <- coef(best_mod_refit)$species
    colnames(params) <- letters[1:ncol(params)]
    # Correction factor
    if (best_mod_formula %in% c("loglog", "expo")) {
      # for transformed (loglog or exp) models
      sp_rmse <- sjstats::rmse(best_mod_refit)
      cf <- exp((sp_rmse^2)/2)
      params[, "a"] <- params[, "a"] + log(cf)  #directly adjust intercept with cf
    } else {
      # for non-transformed models
      cf <- 1
    }
    sp_info <-
      data.frame(species = rownames(params),
                 modelcode = best_mod_name,
                 params,
                 row.names = NULL)

    # Compile outputs
    out <- list()
    out$models_rank <- mix_comp[, mix_comp_cols_print]
    out$best_model <- best_mod_refit
    out$R2 <- MuMIn::r.squaredGLMM(best_mod_refit)
    out$CF <- cf
    out$warnings <- mix_msg

    return(out)

  }
