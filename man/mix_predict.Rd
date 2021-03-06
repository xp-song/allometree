% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mix_predict.R
\name{mix_predict}
\alias{mix_predict}
\title{Make predictions for all species in a dataset using linear mixed-effects model}
\usage{
mix_predict(
  data,
  modelselect,
  level = 0.95,
  stat = "median",
  n.sims = 1000,
  predictor = "diameter",
  species = "species",
  ...
)
}
\arguments{
\item{data}{Dataframe with columns containing the species and variables of
interest. Each row is a measurement for an individual tree.}

\item{modelselect}{Output from the \code{\link[=mix_modelselect]{mix_modelselect()}} function.}

\item{level}{Level of confidence for the prediction interval. Defaults to
\code{0.95}.}

\item{stat}{Specify whether the \code{"median"} or \code{"mean"} of simulated intervals are used.}

\item{n.sims}{Number of bootstrapped simulations to generate the prediction intervals. Defaults to \code{1000}.}

\item{predictor}{Column name of the predictor variable in \code{data}. Defaults to
\code{diameter}.}

\item{species}{Column name of the species variable in \code{data}. Defaults to \code{species}.}

\item{...}{Additional arguments passed to \code{\link[merTools:predictInterval]{merTools::predictInterval()}}}
}
\value{
Dataframe of input \code{data} with columns appended: \describe{
\item{fit}{Predicted value for the response variable.}
\item{lwr}{Lower bound of the prediction interval, based on the input argument \code{level}.}
\item{upr}{Upper bound of the prediction interval, based on the input argument \code{level}.} }
}
\description{
Make predictions across rows in a dataset that may contain multiple species.
The mixed-effects model is used to predict values for the response variable,
as well as it's prediction interval. Necessary bias-corrections are made if
the mixed-effects model has a transformed response variable.
}
\examples{
data(urbantrees)

\dontrun{
model <- mix_modelselect(urbantrees)

Alb_sam <- urbantrees[urbantrees$species == 'Albizia saman', ]  # use one species as an example
results <- mix_predict(data = Alb_sam, modelselect = model,
                       predictor = "diameter") # make predictions for measured values

head(results)
}

}
\seealso{
\code{\link[merTools:predictInterval]{merTools::predictInterval()}} to make predictions from models fit with the \code{lme4} package.

Other mixed-effects model functions: 
\code{\link{mix_modelselect}()},
\code{\link{mix_simulate}()}
}
\concept{mixed-effects model functions}
