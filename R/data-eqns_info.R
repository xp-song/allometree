#' Allometric equations for linear models
#'
#' Allometric equations that are considered in the development of
#' linear models.
#'
#' Six allometric equations used to develop linear models for
#' urban trees:
#'
#' 1. Linear:  \eqn{y_{i} = a + bx_{i} + \epsilon_{i}/\sqrt{w_{i}}}
#' 2. Quadratic: \eqn{y_{i} = a + bx_{i} + cx^2_{i} +
#' \epsilon_{i}/\sqrt{w_{i}}}
#' 3. Cubic: \eqn{y_{i} = a + bx_{i} + cx^2_{i} + dx^3_{i} +
#' \epsilon_{i}/\sqrt{w_{i}}}
#' 4. Quartic: \eqn{y_{i} = a + bx_{i} + cx^2_{i} + dx^3_{i} + ex^4_{i} +
#' \epsilon_{i}/\sqrt{w_{i}}}
#' 5. Log-log: \eqn{\log{(y_{i})} = a + b(\log{(\log{(x_{i} + 1)})}) +
#' \epsilon_{i}/\sqrt{w_{i}}}
#' 6. Exponential: \eqn{\log{(y_{i})} = a + bx_{i} + 1 +
#' \epsilon_{i}/\sqrt{w_{i}}}
#'
#' where
#'
#' \eqn{y_{i}} = Response variable of individual tree \eqn{i},
#' \eqn{i = 1,2,3}... \eqn{n}, \eqn{n} = number of observations
#'
#' \eqn{x_{i}} = Predictor variable
#'
#' \eqn{a,b,c,d,e} = Parameters to be estimated
#'
#' \eqn{\epsilon_{i}} = Normally distributed error term
#'
#' \eqn{w_{i}} = Known weight that takes one of the four
#' following forms: \eqn{w_{i} = 1}, \eqn{w_{i} = 1/\sqrt{x_{i}}},
#' \eqn{w_{i} = 1/\sqrt{x_{i}}}, \eqn{w_{i} = 1/x_{i}^2}.
#'
#' @docType data
#'
#' @usage data(eqns_info)
#'
#' @format A dataframe with 5 variables:
#' \describe{
#'   \item{modeltype}{One of the six general types of allometric equations}
#'   \item{base_equation}{Basic form of the allometric equation}
#'   \item{base_formula}{The basic `formula` used to fit the model}
#'   \item{weights}{The `weight` argument in the model}
#'   \item{modelcode}{Character string used to represent the unique combinations of equations and weights}
#'  }
#'
#' @references McPherson E. G., van Doorn N. S. & Peper P. J. (2016) Urban Tree
#' Database and Allometric Equations. *General Technical Report PSW-GTR-253,
#' USDA Forest Service*, 86.
#'
#' @keywords datasets
#'
#' @examples
#' data(eqns_info)
#' head(eqns_info)
"eqns_info"
