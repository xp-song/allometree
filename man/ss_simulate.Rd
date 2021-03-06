% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_simulate.R
\name{ss_simulate}
\alias{ss_simulate}
\title{Make predictions on simulated data using single-species linear models}
\usage{
ss_simulate(
  ref_table,
  models,
  select_sp = NULL,
  level = 0.95,
  length.out = 100,
  extrapolate = NULL,
  species = "species",
  predictor_min = "predictor_min",
  predictor_max = "predictor_max",
  response_min = "response_min",
  response_max = "response_max",
  cf = "correctn_factor",
  geom_mean = "response_geom_mean"
)
}
\arguments{
\item{ref_table}{Dataframe containing model information. It should include
columns for \code{species}, \code{predictor_min}, \code{predictor_max}, \code{cf}, and
\code{geom_mean}.}

\item{models}{A named list of each species' linear regression models.
\code{names(models)} should correspond to species names in \code{ref_table}.}

\item{select_sp}{Character vector of species names, if you want to run this
function only for selected species in \code{ref_table}. Defaults to \code{NULL}, to
run function across all species.}

\item{level}{Level of confidence for the prediction interval. Defaults to
\code{0.95}.}

\item{length.out}{Number of new predictor values to generate for each species.
Defaults to 100. Set a higher value for greater resolution at the cost of computational time.}

\item{extrapolate}{Numeric vector of 2 elements (e.g. \code{c(0,4)}), representing
the upper and lower bounds of extrapolation. Defaults to \code{NULL} for no
extrapolation.}

\item{species}{Column name in \code{ref_table} for the name of species. Defaults to
\code{species}.}

\item{predictor_min}{Column name in \code{ref_table} for minimum value of the
predictor variable used to fit the model. Defaults to \code{predictor_min}.}

\item{predictor_max}{Column name in \code{ref_table} for maximum value of the
predictor variable used to fit the model. Defaults to \code{predictor_max}.}

\item{response_min}{Column name in \code{ref_table} for minimum value of the
response variable used to fit the model. Defaults to \code{response_min}.}

\item{response_max}{Column name in \code{ref_table} for maximum value of the
response variable used to fit the model. Defaults to \code{response_max}.}

\item{cf}{Column name in \code{ref_table} for the bias correction factor. Defaults
to \code{correctn_factor}.}

\item{geom_mean}{Column name in \code{ref_table} for the geometric mean of response
variable that was used in to fit the \code{models}. Defaults to
\code{response_geom_mean}.}
}
\value{
A dataframe with columns: \describe{
\item{species}{Name of tree species.}
\item{predictor}{Variable used to make predictions.}
\item{fit}{Predicted value.}
\item{lwr}{Lower bound of the prediction interval, based on the input argument \code{level}.}
\item{upr}{Upper bound of the prediction interval, based on the input argument \code{level}.}
\item{extrapolated}{Indicates whether the predictions are based on
extrapolated values. Either 'High', 'Low', or 'No' (not extrapolated).} }
}
\description{
Wrapper function that runs \code{ss_predict()} on simulated data. Data is simulated
for each species based on the range of the predictor variable used to fit the
model, which can be extrapolated to values defined by the user. The
corresponding model object in \code{models} will be used to make predictions on the
simulated data. The user may choose to run this function on one or multiple
species, using the argument \code{select_sp}.
}
\details{
The model associated with each species is used to predict values for the
response variable, as well as it's prediction interval. Necessary
bias-corrections are made for species with models that have a transformed
response variable.
}
\examples{
# first select best-fit model for all species in data
data(urbantrees)
results <- ss_modelselect_multi(urbantrees, species = 'species',
                                response = 'height', predictor = 'diameter')

\dontrun{
# simulate for all species
ss_simulate(ref_table = results$ss_models_info,
            models = results$ss_models)

# simulate for selected species
ss_simulate(ref_table = results$ss_models_info,
            models = results$ss_models,
            selected_spp = 'Albizia saman')

# simulate with extrapolated values
ss_simulate(ref_table = results$ss_models_info,
            models = results$ss_models,
            extrapolate = c(0,3))
}

}
\seealso{
\code{\link[=ss_predict]{ss_predict()}} to make predictions for all species in a dataset using
single-species linear models.

Other single-species model functions: 
\code{\link{ss_modelfit_multi}()},
\code{\link{ss_modelfit}()},
\code{\link{ss_modelselect_multi}()},
\code{\link{ss_modelselect}()},
\code{\link{ss_predict}()}
}
\concept{single-species model functions}
