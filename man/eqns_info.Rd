% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-eqns_info.R
\docType{data}
\name{eqns_info}
\alias{eqns_info}
\title{Allometric equations for linear models}
\format{
A dataframe with 5 variables:
\describe{
\item{modeltype}{One of the six general types of allometric equations.}
\item{base_equation}{Basic form of the allometric equation.}
\item{base_formula}{The basic \code{formula} used to fit the model.}
\item{weights}{The \code{weight} argument in the model.}
\item{modelcode}{Character string used to represent the unique combinations of equations and weights.}
}
}
\usage{
eqns_info
}
\description{
Allometric equations that are considered in the development of
linear models.
}
\details{
Six allometric equations used to develop linear models for
urban trees:
\enumerate{
\item Linear:  \eqn{y_{i} = a + bx_{i} + \epsilon_{i}/\sqrt{w_{i}}}
\item Quadratic: \eqn{y_{i} = a + bx_{i} + cx^2_{i} +
\epsilon_{i}/\sqrt{w_{i}}}
\item Cubic: \eqn{y_{i} = a + bx_{i} + cx^2_{i} + dx^3_{i} +
\epsilon_{i}/\sqrt{w_{i}}}
\item Quartic: \eqn{y_{i} = a + bx_{i} + cx^2_{i} + dx^3_{i} + ex^4_{i} +
\epsilon_{i}/\sqrt{w_{i}}}
\item Log-log: \eqn{\log{(y_{i})} = a + b(\log{(\log{(x_{i} + 1)})}) +
\epsilon_{i}/\sqrt{w_{i}}}
\item Exponential: \eqn{\log{(y_{i})} = a + bx_{i} + 1 +
\epsilon_{i}/\sqrt{w_{i}}}
}

where

\eqn{y_{i}} = Response variable of individual tree \eqn{i},
\eqn{i = 1,2,3}... \eqn{n}, \eqn{n} = number of observations

\eqn{x_{i}} = Predictor variable

\eqn{a,b,c,d,e} = Parameters to be estimated

\eqn{\epsilon_{i}} = Normally distributed error term

\eqn{w_{i}} = Known weight that takes one of the four
following forms: \eqn{w_{i} = 1}, \eqn{w_{i} = 1/\sqrt{x_{i}}},
\eqn{w_{i} = 1/\sqrt{x_{i}}}, \eqn{w_{i} = 1/x_{i}^2}.
}
\examples{
data(eqns_info)
head(eqns_info)
}
\references{
McPherson E. G., van Doorn N. S. & Peper P. J. (2016) Urban Tree
Database and Allometric Equations. \emph{General Technical Report PSW-GTR-253,
USDA Forest Service}, 86.
}
\keyword{datasets}
