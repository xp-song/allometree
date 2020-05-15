#' Select a best-fit linear model for one species
#'
#' Select a best-fit equation for one species, based on the lowest
#' bias-corrected Aikaike’s information criterion (AICc).
#'
#' All allometric equations considered (and ranked) can be found in `?eqns_info`
#' and `data(eqns_info)`. To make the AICc values of equations with a
#' transformed response variable comparable to untransformed equations,
#' \eqn{log(y_{i})} is multiplied by the geometric mean of the response variable
#' in `data`.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree.
#' @param response Column name of the response variable. Defaults to `height`.
#' @param predictor Column name of the predictor variable. Defaults to
#'   `diameter`.
#'
#' @return A list of 3 elements: \describe{ \item{all_models_rank}{Table showing
#'   models ranked by AICc value.} \item{best_model}{Best-fit model object.}
#'   \item{best_model_info}{Table showing information on the best-fit model.} }
#'
#'   ## best_model_info
#'
#'   A dataframe with the following variables: \describe{ \item{modelcode}{Model
#'   code for the best-fit equation.} \item{a, b, c, d, e}{Parameter estimates.}
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
#' @references McPherson E. G., van Doorn N. S. & Peper P. J. (2016) Urban Tree
#'   Database and Allometric Equations. *General Technical Report PSW-GTR-253,
#'   USDA Forest Service*, 86.
#'
#'   Xiao, X., White, E. P., Hooten, M. B., & Durham, S. L. (2011). On the use
#'   of log-transformation vs. nonlinear regression for analyzing biological
#'   power laws. *Ecology*, **92**(10), 1887–1894.
#'
#'   Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference:
#'   Understanding AIC and BIC in model selection. *Sociological Methods and
#'   Research*, **33**(2), 261–304.
#'
#' @family single-species model functions
#' @seealso [sp_modelselect_multi()] to select best-fit models across multiple
#'   species.
#'
#'   [sp_modelfit()] to fit a pre-selected model for one species.
#'
#'   [sp_modelfit_multi()] to fit pre-selected models across multiple species.
#'
#' @examples
#' data(urbantrees)
#' Alb_sam <- urbantrees[urbantrees$species == 'Albizia saman', ]  # subset data for 1 species
#' results <- sp_modelselect(Alb_sam, response = 'height', predictor = 'diameter')
#'
#' head(results$all_models_rank)
#'
#' results$best_model
#'
#' results$best_model_info
#'
#' @import checkmate
#' @importFrom MuMIn AICc
#' @importFrom sjstats rmse
#'
#' @export
sp_modelselect <- function(data, response = "height", predictor = "diameter") {

    # Error checking ------------------
    if (is.null(data[[response]])) {
        stop("Variable name assigned to 'response' not found in 'data'.")
    }
    if (is.null(data[[predictor]])) {
        stop("Variable name assigned to 'predictor' not found in 'data'.")
    }

    # positive numeric variables
    coll <- checkmate::makeAssertCollection()
    checkmate::assert_data_frame(data, add = coll)
    checkmate::assert_subset(response, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(predictor, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::assert_numeric(data[[response]], lower = 1e-05, finite = TRUE, .var.name = "response", add = coll)
    checkmate::assert_numeric(data[[predictor]], lower = 1e-05, finite = TRUE, .var.name = "predictor", add = coll)
    checkmate::reportAssertions(coll)

    # remove missing data
    if (checkmate::anyMissing(data[[response]]) | checkmate::anyMissing(data[[predictor]])) {
        message(cat(sum(!complete.cases(data[, c(response, predictor)])), " row(s) with missing value(s) removed from 'data'", sep = ""))
        data <- data[complete.cases(data[, c(response, predictor)]), ]
    }

    # Calculations ------------------

    # Calculate geometric mean height
    geom_mean_y <- exp(mean(log(data[[response]])))
    data$y_aic <- log(data[[response]]) * geom_mean_y  # * geom_mean_y for AICc comparisons with non-transformed models
    data$y_trans <- log(data[[response]]) # for reporting

    # extract values to use in lm
    y <- data[[response]]
    x <- data[[predictor]]
    y_aic <- data$y_aic
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

    # using y_aic
    sp_model_list$loglog_w1 <- lm(y_aic ~ I(log(log(x + 1))))
    sp_model_list$loglog_w2 <- lm(y_aic ~ I(log(log(x + 1))), weights = I(1/sqrt(x)))
    sp_model_list$loglog_w3 <- lm(y_aic ~ I(log(log(x + 1))), weights = I(1/x))
    sp_model_list$loglog_w4 <- lm(y_aic ~ I(log(log(x + 1))), weights = I(1/x^2))

    sp_model_list$expo_w1 <- lm(y_aic ~ x)
    sp_model_list$expo_w2 <- lm(y_aic ~ x, weights = I(1/sqrt(x)))
    sp_model_list$expo_w3 <- lm(y_aic ~ x, weights = I(1/x))
    sp_model_list$expo_w4 <- lm(y_aic ~ x, weights = I(1/x^2))

    # compare AICc
    all_models_rank <- MuMIn::AICc(sp_model_list$lin_w1, sp_model_list$lin_w2, sp_model_list$lin_w3, sp_model_list$lin_w4, sp_model_list$quad_w1,
        sp_model_list$quad_w2, sp_model_list$quad_w3, sp_model_list$quad_w4, sp_model_list$cub_w1, sp_model_list$cub_w2, sp_model_list$cub_w3,
        sp_model_list$cub_w4, sp_model_list$quart_w1, sp_model_list$quart_w2, sp_model_list$quart_w3, sp_model_list$quart_w4, sp_model_list$loglog_w1,
        sp_model_list$loglog_w2, sp_model_list$loglog_w3, sp_model_list$loglog_w4, sp_model_list$expo_w1, sp_model_list$expo_w2, sp_model_list$expo_w3,
        sp_model_list$expo_w4)

    # re-fit transformed models with y_trans instead of y_aic
    sp_model_list$loglog_w1 <- lm(y_trans ~ I(log(log(x + 1))))
    sp_model_list$loglog_w2 <- lm(y_trans ~ I(log(log(x + 1))), weights = I(1/sqrt(x)))
    sp_model_list$loglog_w3 <- lm(y_trans ~ I(log(log(x + 1))), weights = I(1/x))
    sp_model_list$loglog_w4 <- lm(y_trans ~ I(log(log(x + 1))), weights = I(1/x^2))
    sp_model_list$expo_w1 <- lm(y_trans ~ x)
    sp_model_list$expo_w2 <- lm(y_trans ~ x, weights = I(1/sqrt(x)))
    sp_model_list$expo_w3 <- lm(y_trans ~ x, weights = I(1/x))
    sp_model_list$expo_w4 <- lm(y_trans ~ x, weights = I(1/x^2))

    # Prepare output ----------------------

    # extract AICc comparison table
    all_models_rank$model <- names(sp_model_list)
    all_models_rank <- all_models_rank[order(all_models_rank$AICc), ]
    rownames(all_models_rank) <- NULL

    # extract best model object
    best_model <- sp_model_list[names(sp_model_list) == all_models_rank$model[1]][[1]]

    # extract best model info
    best_model_info <- data.frame(modelcode = all_models_rank$model[order(all_models_rank$AICc)][1])
    best_model_info$a <- summary(best_model)$coef[, "Estimate"][[1]]
    best_model_info$b <- summary(best_model)$coef[, "Estimate"][[2]]

    # include other parameters (NA if absent)
    best_model_info$c <- tryCatch(summary(best_model)$coef[, "Estimate"][[3]], error = function(e) NA)
    best_model_info$d <- tryCatch(summary(best_model)$coef[, "Estimate"][[4]], error = function(e) NA)
    best_model_info$e <- tryCatch(summary(best_model)$coef[, "Estimate"][[5]], error = function(e) NA)

    best_model_info$response_geom_mean <- geom_mean_y

    # correction factor
    if ("y_trans" %in% names(best_model$model)) {
        # for transformed (loglog or exp) models
        sp_rmse <- sjstats::rmse(best_model)/geom_mean_y
        cf <- exp((sp_rmse^2)/2)

        best_model_info$correctn_factor <- cf
        best_model_info$a <- best_model_info$a + log(cf)  #directly adjust intercept with cf
    } else {
        # for non-transformed models
        best_model_info$correctn_factor <- 1
    }

    best_model_info$predictor_min <- min(data[[predictor]])
    best_model_info$predictor_max <- max(data[[predictor]])
    best_model_info$response_min <- min(data[[response]])
    best_model_info$response_max <- max(data[[response]])

    best_model_info$residual_SE <- round(summary(best_model)$sigma, 4)
    best_model_info$mean_SE <- round(mean(residuals(best_model)^2), 4)
    best_model_info$adj_R2 <- round(summary(best_model)$adj.r.squared, 4)
    # best_model_info$F.statistic <- summary(best_model)$fstatistic[[1]]
    best_model_info$n <- nrow(data)


    # combine output in list
    output <- list(all_models_rank, best_model, best_model_info)
    names(output) <- c("all_models_rank", "best_model", "best_model_info")

    return(output)
}



#' Select best-fit linear models across multiple species
#'
#' Wrapper function that runs `sp_modelselect()` across multiple species in a
#' dataframe. A single best-fit equation is selected per species, based on the
#' lowest AICc value. All allometric equations considered (and ranked) can be
#' found in `?eqns_info` and `data(eqns_info)`.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree of a particular species.
#' @param species Column name of the species variable. Defaults to `species`.
#' @param response Column name of the response variable. Defaults to `height`.
#' @param predictor Column name of the predictor variable. Defaults to
#'   `diameter`.
#'
#' @return A list of 3 elements:  \describe{ \item{sp_models_rank}{List of
#'   tables showing each species' candidate models ranked by AICc value.}
#'   \item{sp_models}{List of each species' best-fit model object.}
#'   \item{sp_models_info}{Table showing each species' best-fit model
#'   information.} }
#'
#'   ## sp_models_info
#'
#'   A dataframe with the following variables: \describe{ \item{species}{Name of
#'   tree species.} \item{modelcode}{Model code for the best-fit equation.}
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
#' @family single-species model functions
#' @seealso [sp_modelselect()] to select a best-fit model for one species.
#'
#'   [sp_modelfit()] to fit a pre-selected model for one species.
#'
#'   [sp_modelfit_multi()] to fit pre-selected models across multiple species.
#'
#' @examples
#' data(urbantrees)
#' results <- sp_modelselect_multi(urbantrees, species = 'species',
#'                                 response = 'height', predictor = 'diameter')
#'
#' head(results$sp_models_rank[[1]]) # Highly-ranked models for 1st species in list
#'
#' results$sp_models[[1]] # model object for 1st species in list
#'
#' results$sp_models_info # summary of best-fit models
#'
#' @import checkmate
#' @importFrom stats complete.cases lm residuals
#'
#' @export
sp_modelselect_multi <- function(data, species = "species", response = "height", predictor = "diameter") {

    # Error checking ------------------

    # check species variable ref_table needs 'modelcode' variable
    if (is.null(data[[species]])) {
        stop("Variable name assigned to 'species' not found in 'data'.")
    }
    coll <- checkmate::makeAssertCollection()
    checkmate::assert_subset(species, choices = colnames(data), empty.ok = FALSE, add = coll)
    checkmate::reportAssertions(coll)
    checkmate::assert(checkmate::check_character(data[[species]]), checkmate::check_factor(data[[species]]), combine = "or", .var.name = "species")

    # Calculations ------------------

    data_list <- split(data, data[[species]])  #split df into list of species

    # create empty dfs to collate info in loop
    sp_models_info <- data.frame(species = as.character(), modelcode = as.character(), a = as.numeric(), b = as.numeric(), c = as.numeric(),
        d = as.numeric(), e = as.numeric(), response_geom_mean = as.numeric(), correctn_factor = as.numeric(), predictor_max = as.numeric(),
        predictor_min = as.numeric(), response_min = as.numeric(), response_max = as.numeric(), residual_SE = as.numeric(), mean_SE = as.numeric(),
        adj_R2 = as.numeric(), n = as.numeric())

    sp_models <- list()
    sp_models_rank <- list()


    for (i in 1:length(data_list)) {
        results <- sp_modelselect(data_list[[i]], response, predictor)

        # extract model ranks
        sp_models_rank[i] <- list(results[[1]])
        names(sp_models_rank)[i] <- names(data_list)[i]

        # extract model object
        sp_models[i] <- list(results[[2]])
        names(sp_models)[i] <- names(data_list)[i]

        # extract model info
        append <- cbind.data.frame(data.frame(species = names(data_list)[i]), results[[3]])
        sp_models_info <- rbind(sp_models_info, append)

    }

    # combine output in list
    output <- list(sp_models_rank, sp_models, sp_models_info)
    names(output) <- c("sp_models_rank", "sp_models", "sp_models_info")

    return(output)
}
