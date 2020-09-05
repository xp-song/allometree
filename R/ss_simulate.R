#'Make predictions on simulated data using single-species linear models
#'
#'Wrapper function that runs `ss_predict()` on simulated data. Data is simulated
#'for each species based on the range of the predictor variable used to fit the
#'model, which can be extrapolated to values defined by the user. The
#'corresponding model object in `models` will be used to make predictions on the
#'simulated data. The user may choose to run this function on one or multiple
#'species, using the argument `select_sp`.
#'
#'The model associated with each species is used to predict values for the
#'response variable, as well as it's prediction interval. Necessary
#'bias-corrections are made for species with models that have a transformed
#'response variable.
#'
#'@param ref_table Dataframe containing model information. It should include
#'  columns for `species`, `predictor_min`, `predictor_max`, `cf`, and
#'  `geom_mean`.
#'@param models A named list of each species' linear regression models.
#'  `names(models)` should correspond to species names in `ref_table`.
#'@param select_sp Character vector of species names, if you want to run this
#'  function only for selected species in `ref_table`. Defaults to `NULL`, to
#'  run function across all species.
#'@param level Level of confidence for the prediction interval. Defaults to
#'  `0.95`.
#'@param length.out Number of new predictor values to generate for each species.
#'Defaults to 100. Set a higher value for greater resolution at the cost of computational time.
#'@param extrapolate Numeric vector of 2 elements (e.g. `c(0,4)`), representing
#'  the upper and lower bounds of extrapolation. Defaults to `NULL` for no
#'  extrapolation.
#'@param species Column name in `ref_table` for the name of species. Defaults to
#'  `species`.
#'@param predictor_min Column name in `ref_table` for minimum value of the
#'  predictor variable used to fit the model. Defaults to `predictor_min`.
#'@param predictor_max Column name in `ref_table` for maximum value of the
#'  predictor variable used to fit the model. Defaults to `predictor_max`.
#'@param response_min Column name in `ref_table` for minimum value of the
#'  response variable used to fit the model. Defaults to `response_min`.
#'@param response_max Column name in `ref_table` for maximum value of the
#'  response variable used to fit the model. Defaults to `response_max`.
#'@param cf Column name in `ref_table` for the bias correction factor. Defaults
#'  to `correctn_factor`.
#'@param geom_mean Column name in `ref_table` for the geometric mean of response
#'  variable that was used in to fit the `models`. Defaults to
#'  `response_geom_mean`.
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
#'@family single-species model functions
#'@seealso [ss_predict()] to make predictions for all species in a dataset using
#'  single-species linear models.
#'
#' @examples
#' # first select best-fit model for all species in data
#' data(urbantrees)
#' results <- ss_modelselect_multi(urbantrees, species = 'species',
#'                                 response = 'height', predictor = 'diameter')
#'
#' \dontrun{
#' # simulate for all species
#' ss_simulate(ref_table = results$ss_models_info,
#'             models = results$ss_models)
#'
#' # simulate for selected species
#' ss_simulate(ref_table = results$ss_models_info,
#'             models = results$ss_models,
#'             selected_spp = 'Albizia saman')
#'
#' # simulate with extrapolated values
#' ss_simulate(ref_table = results$ss_models_info,
#'             models = results$ss_models,
#'             extrapolate = c(0,3))
#' }
#'
#'@import checkmate
#'@import magrittr
#'@import dplyr
#'@importFrom tidyr pivot_longer
#'@importFrom stats complete.cases
#'@importFrom rlang .data
#'
#'@export
ss_simulate <- function(ref_table, models, select_sp = NULL, level = 0.95, length.out = 100, extrapolate = NULL, species = "species", predictor_min = "predictor_min",
    predictor_max = "predictor_max", response_min = "response_min", response_max = "response_max", cf = "correctn_factor", geom_mean = "response_geom_mean") {

    # Error checking ------------------
    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assert_data_frame(ref_table, add = coll)
    checkmate::assert_list(models, unique = TRUE, types = "list", add = coll)

    # colnames
    checkmate::assert_subset(species, choices = colnames(ref_table), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(cf, choices = colnames(ref_table), empty.ok = FALSE, add = coll)
    checkmate::assert_subset(geom_mean, choices = colnames(ref_table), empty.ok = FALSE, add = coll)

    # species in ref_table also in names(models)
    checkmate::assert_subset(as.character(ref_table[[species]]), choices = names(models), empty.ok = FALSE, add = coll)

    # confidence level
    checkmate::assert_number(level, lower = 0, upper = 1, add = coll)

    # length.out
    checkmate::assert_integerish(length.out, lower = 1, add = coll)

    if (!is.null(select_sp)) {
        # run if argument select_sp is present

        checkmate::assert_subset(select_sp, choices = as.character(ref_table[[species]]), empty.ok = TRUE, add = coll)
        ref_table <- ref_table[ref_table[[species]] %in% select_sp, ]
    }

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    predict_range <- ref_table[names(ref_table) %in% c(species, predictor_min, predictor_max)]
    predict_range$species <- as.character(predict_range$species)  # to avoid empty lists in next step
    predict_range_full <- apply(predict_range, 1, function(x) seq(x[predictor_min], x[predictor_max], length.out = length.out))  #each sp is a column
    predict_range_full <- as.data.frame(predict_range_full)

    colnames(predict_range_full) <- predict_range$species
    predict_range_full <- tidyr::pivot_longer(predict_range_full, cols = colnames(predict_range_full), names_to = "species", values_to = "predictor")
    # predict_range_full <- predict_range_full[order(predict_range_full$species, predict_range_full$predictor),]

    # run ss_predict
    output <- ss_predict(predict_range_full, models, ref_table, level = level, species = "species", predictor = "predictor", cf = "correctn_factor",
        geom_mean = "response_geom_mean")

    # classify extrapolated for response variable
    response_ranges <- ref_table[, colnames(ref_table) %in% c(species, response_min, response_max)]
    response_ranges$species <- as.character(response_ranges$species)
    colnames(response_ranges) <- c("species", "ymin", "ymax")  # avoid confusion with function arguments

    output <- output %>% dplyr::left_join(response_ranges, by = c(species = "species")) %>% dplyr::group_by(species) %>% dplyr::mutate(extrapolated = ifelse(.data$fit <
        .data$ymin, "Low", ifelse(.data$fit > .data$ymax, "High", "No"))) %>% dplyr::select(-.data$ymin, -.data$ymax)

    # extrapolated predictor values
    if (!is.null(extrapolate)) {
      checkmate::assert_numeric(extrapolate, len = 2, lower = 0, sorted = TRUE, any.missing = FALSE)

        # extrapolate[1] to predictor_min
        predict_range_low <- apply(predict_range, 1, function(x) if (x[predictor_min] > extrapolate[1]) {
            seq(extrapolate[1], x[predictor_min], length.out = length.out)
          } else {
            rep(NA, length.out)
            })  #each sp is a column
        if (!is.null(predict_range_low)) {
            predict_range_low <- as.data.frame(predict_range_low)
            colnames(predict_range_low) <- predict_range$species

            predict_range_low <- tidyr::pivot_longer(predict_range_low, cols = colnames(predict_range_low), names_to = "species", values_to = "predictor")
            predict_range_low$extrapolated <- "Low"
            }

        # extrapolate[2] to predictor_max
        predict_range_high <- apply(predict_range, 1, function(x) if (x[predictor_max] < extrapolate[2]) {
            seq(extrapolate[2], x[predictor_max], length.out = length.out)
        } else {
            rep(NA, length.out)
        })  #each sp is a column

        if (!is.null(predict_range_high)) {
            predict_range_high <- as.data.frame(predict_range_high)
            colnames(predict_range_high) <- predict_range$species
            predict_range_high <- tidyr::pivot_longer(predict_range_high, cols = colnames(predict_range_high), names_to = "species", values_to = "predictor")
            predict_range_high$extrapolated <- "High"
        }

        predict_range_extra <- rbind.data.frame(predict_range_low, predict_range_high)
        predict_range_extra <- unique(predict_range_extra)  # remove duplicated rows
        predict_range_extra <- predict_range_extra[complete.cases(predict_range_extra), ]


        output_extra <- ss_predict(predict_range_extra, models, ref_table, level = level, species = "species", predictor = "predictor", cf = "correctn_factor",
            geom_mean = "response_geom_mean")

        output <- rbind.data.frame(output, output_extra)
    }

    output <- output[complete.cases(output), ]

    return(output)
}
