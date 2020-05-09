#' Fit data to a specified linear model for one species
#'
#' Fit data to a specified allometric equation, for one species. Allometric
#' equations that may be considered as an input to this function can be found in
#' `?eqns_info` and `data(eqns_info)`.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree.
#' @param modelcode Character string of the model code for the selected
#'   allometric equation. Refer to `?eqns_info` and `data(eqns_info)` for more
#'   information.
#' @param response Column name of the response variable. Defaults to `height`.
#' @param predictor Column name of the predictor variable. Defaults to
#'   `diameter`.
#'
#' @return A list of 2 elements: \describe{ \item{fitted_model}{Resulting model
#'   object.} \item{fitted_model_info}{Table showing information on the
#'   resulting model.} }
#'
#'   ## fitted_model_info
#'
#'   A dataframe with the following variables: \describe{ \item{modelcode}{Model
#'   code for the allometric equation used.} \item{a, b, c, d, e}{Parameter
#'   estimates.} \item{response_geom_mean}{Geometric mean of the response
#'   variable used in calculation of AICc (only for transformed models).}
#'   \item{correctn_factor}{Bias correction factor to use on model predictions
#'   (only for transformed models).} \item{predictor_min, predictor_max}{Range of
#'   the predictor variable within the data used to generate the model.}
#'   \item{response_min, response_max}{Range of the response variable within the
#'   data used to generate the model.} \item{residual_SE}{Residual standard error
#'   of the model.} \item{mean_SE}{Mean standard error of the model.}
#'   \item{adj_R2}{Adjusted \eqn{R^2} of the model.} \item{n}{Sample size (no. of
#'   trees used to fit model).} }
#'
#' @references McPherson E. G., van Doorn N. S. & Peper P. J. (2016) Urban Tree
#'   Database and Allometric Equations. *General Technical Report PSW-GTR-253,
#'   USDA Forest Service*, 86.
#'
#' @family single-species model functions
#' @seealso [sp_modelfit_multi()] to fit specified models across multiple
#'   species.
#'
#'   [sp_modelselect()] to select a best-fit model for one species.
#'
#'   [sp_modelselect_multi()] to select best-fit models across multiple species.
#'
#' @examples
#' data(urbantrees)
#' Alb_sam <- urbantrees[urbantrees$species == 'Albizia saman', ]
#' results <- sp_modelfit(Alb_sam,
#'                        modelcode = 'quad_w1', # manually specify equation to use
#'                        response = 'height', predictor = 'diameter')
#'
#' results$fitted_model
#'
#' results$fitted_model_info
#'
#' @import checkmate
#' @importFrom sjstats rmse
#' @importFrom stats complete.cases lm residuals
#'
#' @export

sp_modelfit <- function(data, modelcode, response = "height", predictor = "diameter") {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()
    checkmate::assert_subset(response, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(predictor, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_numeric(data[[response]], lower = 1e-05, finite = TRUE, .var.name = "response", add = coll)
    checkmate::assert_numeric(data[[predictor]], lower = 1e-05, finite = TRUE, .var.name = "predictor", add = coll)

    # check model parameter
    checkmate::assert_choice(as.character(modelcode), c("lin_w1", "lin_w2", "lin_w3", "lin_w4", "quad_w1", "quad_w2",
        "quad_w3", "quad_w4", "cub_w1", "cub_w2", "cub_w3", "cub_w4", "quart_w1", "quart_w2", "quart_w3", "quart_w4",
        "loglog_w1", "loglog_w2", "loglog_w3", "loglog_w4", "expo_w1", "expo_w2", "expo_w3", "expo_w4"), .var.name = "modelcode",
        add = coll)

    checkmate::reportAssertions(coll)

    # remove missing data
    if (checkmate::anyMissing(data[[response]]) | checkmate::anyMissing(data[[predictor]])) {
        message(cat(sum(!complete.cases(data[, c(response, predictor)])), " row(s) with missing value(s) removed from 'data'",
            sep = ""))
        data <- data[complete.cases(data[, c(response, predictor)]), ]
    }

    # Calculations ------------------

    # Calculate geometric mean height
    geom_mean_y <- exp(mean(log(data[[response]])))
    data$y_trans <- log(data[[response]]) * geom_mean_y  # use this for transformed models for AICc comparisons with non-transformed models

    # extract values to use in lm
    y <- data[[response]]
    x <- data[[predictor]]
    y_trans <- data$y_trans

    # fit models
    sp_model_list <- list()

    sp_model_list$lin_w1 <- lm(y ~ x)
    sp_model_list$lin_w2 <- lm(y ~ x, weights = I(1/sqrt(x)))
    sp_model_list$lin_w3 <- lm(y ~ x, weights = I(1/x))
    sp_model_list$lin_w4 <- lm(y ~ x, weights = I(1/x^2))

    sp_model_list$quad_w1 <- lm(y ~ x + I(x^2))
    sp_model_list$quad_w2 <- lm(y ~ x + I(x^2), weights = I(1/sqrt(x)))
    sp_model_list$quad_w3 <- lm(y ~ x + I(x^2), weights = I(1/x))
    sp_model_list$quad_w4 <- lm(y ~ x + I(x^2), weights = I(1/x^2))

    sp_model_list$cub_w1 <- lm(y ~ x + I(x^2) + I(x^3))
    sp_model_list$cub_w2 <- lm(y ~ x + I(x^2) + I(x^3), weights = I(1/sqrt(x)))
    sp_model_list$cub_w3 <- lm(y ~ x + I(x^2) + I(x^3), weights = I(1/x))
    sp_model_list$cub_w4 <- lm(y ~ x + I(x^2) + I(x^3), weights = I(1/x^2))

    sp_model_list$quart_w1 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
    sp_model_list$quart_w2 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), weights = I(1/sqrt(x)))
    sp_model_list$quart_w3 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), weights = I(1/x))
    sp_model_list$quart_w4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), weights = I(1/x^2))

    # using y_trans
    sp_model_list$loglog_w1 <- lm(y_trans ~ I(log(log(x + 1))))
    sp_model_list$loglog_w2 <- lm(y_trans ~ I(log(log(x + 1))), weights = I(1/sqrt(x)))
    sp_model_list$loglog_w3 <- lm(y_trans ~ I(log(log(x + 1))), weights = I(1/x))
    sp_model_list$loglog_w4 <- lm(y_trans ~ I(log(log(x + 1))), weights = I(1/x^2))

    sp_model_list$expo_w1 <- lm(y_trans ~ x)
    sp_model_list$expo_w2 <- lm(y_trans ~ x, weights = I(1/sqrt(x)))
    sp_model_list$expo_w3 <- lm(y_trans ~ x, weights = I(1/x))
    sp_model_list$expo_w4 <- lm(y_trans ~ x, weights = I(1/x^2))

    # Prepare output ----------------------

    # extract model object
    fitted_model <- sp_model_list[names(sp_model_list) == modelcode][[1]]

    # extract best model info
    fitted_model_info <- data.frame(modelcode = modelcode)
    fitted_model_info$a <- summary(fitted_model)$coef[, "Estimate"][[1]]
    fitted_model_info$b <- summary(fitted_model)$coef[, "Estimate"][[2]]

    # include other parameters (NA if absent)
    fitted_model_info$c <- tryCatch(summary(fitted_model)$coef[, "Estimate"][[3]], error = function(e) NA)
    fitted_model_info$d <- tryCatch(summary(fitted_model)$coef[, "Estimate"][[4]], error = function(e) NA)
    fitted_model_info$e <- tryCatch(summary(fitted_model)$coef[, "Estimate"][[5]], error = function(e) NA)

    fitted_model_info$response_geom_mean <- geom_mean_y

    # correction factor
    if ("y_trans" %in% names(fitted_model$model)) {
        # for transformed (loglog or exp) models
        sp_rmse <- sjstats::rmse(fitted_model)/geom_mean_y
        cf <- exp((sp_rmse^2)/2)

        fitted_model_info$correctn_factor <- cf
        fitted_model_info$a <- fitted_model_info$a + cf  #directly adjust intercept with cf
    } else {
        # for non-transformed models
        fitted_model_info$correctn_factor <- 1
    }

    fitted_model_info$predictor_min <- min(data[[predictor]])
    fitted_model_info$predictor_max <- max(data[[predictor]])
    fitted_model_info$response_min <- min(data[[response]])
    fitted_model_info$response_max <- max(data[[response]])

    fitted_model_info$residual_SE <- round(summary(fitted_model)$sigma, 4)
    fitted_model_info$mean_SE <- round(mean(residuals(fitted_model)^2), 4)
    fitted_model_info$adj_R2 <- round(summary(fitted_model)$adj.r.squared, 4)
    # fitted_model_info$F.statistic <- summary(fitted_model)$fstatistic[[1]]
    fitted_model_info$n <- nrow(data)


    # combine output in list
    output <- list(fitted_model, fitted_model_info)
    names(output) <- c("fitted_model", "fitted_model_info")

    return(output)
}


#' Fit data to specified linear models across multiple species
#'
#' Wrapper function that runs `sp_modelfit()` across multiple species. Data is
#' fit to specified allometric equations for each species, as defined within
#' `ref_table`. The full list of allometric equations that may be considered in
#' `ref_table` can be found in `?eqns_info` and `data(eqns_info)`.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree of a particular species.
#' @param ref_table Dataframe containing an allometric equation for each tree
#'   species, in the form of a model code. Each row is a unique species.
#' @param species Column name of the species variable in both the dataframes
#'   `data` and `ref_table`. Defaults to `species`.
#' @param modelcode Column name containing the model codes in `ref_table`. Refer
#'   to `data(eqns_info)` for more information on model codes.
#' @param response Column name of the response variable in `data`. Defaults to
#'   `height`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#'
#' @return A list of 2 elements: \describe{ \item{sp_models}{List of each
#'   species' resulting model object.} \item{sp_models_info}{Table showing each
#'   species' resulting model information.} }
#'
#'   ## sp_models_info
#'
#'   A dataframe with the following variables: \describe{ \item{species}{Name of
#'   tree species.} \item{modelcode}{Model code for the allometric equation used.}
#'   \item{a, b, c, d, e}{Parameter estimates.}
#'   \item{response_geom_mean}{Geometric mean of the response variable used in
#'   calculation of AICc (only for transformed models).}
#'   \item{correctn_factor}{Bias correction factor to use on model predictions
#'   (only for transformed models).} \item{predictor_min, predictor_max}{Range of
#'   the predictor variable within the data used to generate the model.}
#'   \item{response_min, response_max}{Range of the response variable within the
#'   data used to generate the model.} \item{residual_SE}{Residual standard error
#'   of the model.} \item{mean_SE}{Mean standard error of the model.}
#'   \item{adj_R2}{Adjusted \eqn{R^2} of the model.} \item{n}{Sample size (no. of
#'   trees used to fit model).} }
#'
#' @examples
#' # first select best-fit model for all species in data
#' data(urbantrees)
#' selected <- sp_modelselect_multi(urbantrees, species = 'species',
#'                                  response = 'height', predictor = 'diameter')
#'
#' # use function
#' results <- sp_modelfit_multi(
#'   urbantrees, # any data with similar species (re-use same data in this case)
#'   ref_table = selected$sp_models_info,
#'   species = 'species', modelcode = 'modelcode',
#'   response = 'height', predictor = 'diameter'
#' )
#'
#' results$sp_models[[1]] # model object for first species in list
#'
#' results$sp_models_info # summary of fitted models
#'
#' @family single-species model functions
#' @seealso [sp_modelfit()] to fit a specified model for one species.
#'
#'   [sp_modelselect()] to select a best-fit model for one species.
#'
#'   [sp_modelselect_multi()] to select best-fit models across multiple species.
#'
#' @import checkmate
#'
#' @export
sp_modelfit_multi <- function(data, ref_table, species = "species", modelcode = "modelcode", response = "height",
    predictor = "diameter") {

    # Error checking ------------------
    coll <- checkmate::makeAssertCollection()

    # species
    checkmate::assert_subset(species, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(species, choices = colnames(ref_table), empty.ok = FALSE, add = coll)
    checkmate::assert(checkmate::check_character(data[[species]]), checkmate::check_factor(data[[species]]),
        combine = "or", .var.name = "species")
    checkmate::assert(checkmate::check_character(ref_table[[species]]), checkmate::check_factor(ref_table[[species]]),
        combine = "or", .var.name = "species")

    if (!setequal(unique(data[[species]]), unique(ref_table[[species]]))) {
        message("Warning: The unique types of 'species' between 'data' and 'ref_table' do not match.")
    }
    if (!all(unique(data[[species]]) %in% unique(ref_table[[species]]))) {
        stop("There are 'species' in the 'data' not found in the 'ref_table'.")
    }

    # modelcode
    checkmate::assert_subset(modelcode, choices = colnames(ref_table), empty.ok = FALSE, add = coll)

    checkmate::reportAssertions(coll)

    # Calculations ------------------

    data_list <- split(data, data[[species]])  #split df into list of species

    # create empty dfs to collate info in loop
    sp_models_info <- data.frame(species = as.character(), modelcode = as.character(), a = as.numeric(), b = as.numeric(),
        c = as.numeric(), d = as.numeric(), e = as.numeric(), response_geom_mean = as.numeric(), correctn_factor = as.numeric(),
        predictor_max = as.numeric(), predictor_min = as.numeric(), response_min = as.numeric(), response_max = as.numeric(),
        residual_SE = as.numeric(), mean_SE = as.numeric(), adj_R2 = as.numeric(), n = as.numeric())

    sp_models <- list()

    for (i in 1:length(data_list)) {

        results <- sp_modelfit(data_list[[i]], modelcode = ref_table[ref_table[[species]] == names(data_list)[i], modelcode],
            response, predictor)

        # extract model object
        sp_models[i] <- list(results[[1]])
        names(sp_models)[i] <- names(data_list)[i]

        # extract model info
        append <- cbind.data.frame(data.frame(species = names(data_list)[i]), results[[2]])
        sp_models_info <- rbind(sp_models_info, append)
    }

    # combine output in list
    output <- list(sp_models, sp_models_info)
    names(output) <- c("sp_models", "sp_models_info")

    return(output)
}
