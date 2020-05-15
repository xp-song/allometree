#' Make predictions for all species in a dataset using single-species linear
#' models
#'
#' Make predictions across rows in a dataset that may contain multiple species.
#' The model associated with each species is used to predict values for the
#' response variable, as well as it's prediction interval. Necessary
#' bias-corrections are made for species with models that have a transformed
#' response variable.
#'
#' @param data Dataframe with columns containing the species and variables of
#'   interest. Species names in the `species` column should be present in those
#'   within `ref_table`, as well as those in `models`.
#' @param models A named list of each species' linear regression models.
#'   `names(models)` should correspond to species names in `data` and
#'   `ref_table`.
#' @param ref_table Dataframe containing information to correct bias introduced
#'   in models with a transformed response variable. It should include columns
#'   for `species`, `cf`, and `geom_mean`.
#' @param level Level of confidence for the prediction interval. Defaults to
#'   `0.95`.
#' @param species Column name of the species variable in `data` and `ref_table`.
#'   Defaults to `species`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#' @param cf Column name of the bias correction factor in `ref_table`. Defaults
#'   to `correctn_factor`.
#' @param geom_mean Column name of the geometric mean of response variable in
#'   `ref_table`, that was used in to fit the `models`. Defaults to
#'   `response_geom_mean`.
#'
#' @return Dataframe of input `data` with columns appended: \describe{
#'   \item{fit}{Predicted value for the response variable.} \item{lwr}{Lower
#'   bound of the prediction interval, based on the input argument `level`.}
#'   \item{upr}{Upper bound of the prediction interval, based on the input
#'   argument `level`.} }
#'
#' @family single-species model functions
#' @seealso [sp_simulate()] to run `sp_predict()` on simulated data.
#'
#' @examples
#' # first select best-fit model
#' data(urbantrees)
#' Alb_sam <- urbantrees[urbantrees$species == 'Albizia saman', ]  # we use one species as an example
#' results <- sp_modelselect_multi(Alb_sam,
#'                                 species = 'species',
#'                                 response = 'height',
#'                                 predictor = 'diameter')
#'
#' # simulate data
#' predict_range <- results$sp_models_info[ ,c('species','predictor_min', 'predictor_max')]
#' predict_range_full <- as.data.frame(apply(predict_range, 1,
#'                                           function(x) seq(x['predictor_min'], x['predictor_max'],
#'                                           length.out = 100)))
#' colnames(predict_range_full) <- predict_range$species
#' predict_range_full <- tidyr::pivot_longer(predict_range_full,
#'                                           cols = colnames(predict_range_full),
#'                                           names_to = 'species',
#'                                           values_to = 'predictor')
#'
#' # run function
#' predictions <- sp_predict(predict_range_full,
#'                           models = results$sp_models,
#'                           ref_table = results$sp_models_info,
#'                           predictor = 'predictor')
#' head(predictions)
#'
#' @import checkmate
#' @importFrom stringr str_replace
#' @importFrom stats as.formula complete.cases predict
#'
#' @export
sp_predict <- function(data, models, ref_table, level = 0.95, species = "species", predictor = "diameter", cf = "correctn_factor", geom_mean = "response_geom_mean") {

    # Error checking ------------------
    coll <- checkmate::makeAssertCollection()
    checkmate::assert_list(models, unique = TRUE, types = "list", add = coll)
    checkmate::assert_data_frame(ref_table, add = coll)
    checkmate::assert_subset(unique(data[[species]]), choices = names(models), empty.ok = FALSE, add = coll)  # species in data in models
    checkmate::assert_subset(unique(data[[species]]), choices = as.character(ref_table[[species]]), empty.ok = FALSE, add = coll)  # species in data in models

    checkmate::assert_number(level, lower = 0, upper = 1, add = coll)
    checkmate::assert_numeric(data[[predictor]], lower = 0, finite = TRUE, .var.name = "predictor", add = coll)
    checkmate::reportAssertions(coll)

    # remove missing data
    if (checkmate::anyMissing(data[[predictor]])) {
        message(cat(sum(!complete.cases(data[, predictor])), " row(s) with missing value(s) removed from 'data'", sep = ""))
        data <- data[complete.cases(data[, predictor]), ]
    }

    # Calculations ------------------

    data_list <- split(data, data$species)

    for (i in 1:length(data_list)) {

        spp <- names(data_list[i])
        index <- which(names(models) == spp)

        data_sub <- data_list[i][[1]]

        data_sub$x <- data_sub[[predictor]]  # IMPORTANT: align variable name with model

        # extract weight info from model object
        if ("weights" %in% names(models[[index]])) {
            w <- as.character(models[[index]]$call)[length(models[[index]]$call)]
            w <- stringr::str_replace(w, "x", "data_sub$x")  # IMPORTANT
            w <- as.formula(paste("~", w))
        } else {
            w <- 1
        }

        results <- predict(models[[index]], newdata = data_sub, level = level, interval = "prediction", type = "response", weights = w)
        results <- as.data.frame(results)


        # Transformed models: Geom mean height
        if (ref_table$modelcode[ref_table$species == spp] == "loglog_w1" | ref_table$modelcode[ref_table$species == spp] == "loglog_w2" | ref_table$modelcode[ref_table$species ==
            spp] == "loglog_w3" | ref_table$modelcode[ref_table$species == spp] == "loglog_w4" | ref_table$modelcode[ref_table$species == spp] ==
            "expo_w1" | ref_table$modelcode[ref_table$species == spp] == "expo_w2" | ref_table$modelcode[ref_table$species == spp] == "expo_w3" |
            ref_table$modelcode[ref_table$species == spp] == "expo_w4") {

            # back-transform & bias-correction
            results$fit <- exp(results$fit) * ref_table$correctn_factor[ref_table$species == spp]
            results$lwr <- exp(results$lwr) * ref_table$correctn_factor[ref_table$species == spp]
            results$upr <- exp(results$upr) * ref_table$correctn_factor[ref_table$species == spp]
        }

        data_list[[i]] <- cbind.data.frame(data_list[[i]], results)
    }

    data_list <- do.call(rbind, data_list)  #combine lists by rows

    data_list <- data_list[complete.cases(data_list), ]

    rownames(data_list) <- NULL

    return(data_list)
}
