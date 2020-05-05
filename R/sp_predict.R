#' Predict using best-fit model for one species
#'
#' [sp_modelselect()] selects a best-fit equation for one species, based on the
#' lowest bias-corrected Aikaike’s information criterion (AICc).
#'
#' All allometric equations considered (and ranked) can be found in
#' `data(eqns_info)`. To make the AICc values of equations with a transformed
#' response variable comparable to untransformed equations, \eqn{log(y_{i})} is
#' multiplied by the geometric mean of the response variable in `data`.
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree.
#' @param

#'
#' @return

#'
#' @family single-species model functions
#' @seealso
#'
#' @examples
#' sp_modelselect(data, response = 'height', predictor = 'diameter')
#'
#' @import checkmate
#' @importFrom MuMIn AICc
#' @importFrom sjstats rmse
#'
#' @export
