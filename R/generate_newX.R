#' Helper function to generate new predictors for each species for simulation/prediction
#'
#' @param data Dataframe that contains the variables of interest. Each row is a
#'   measurement for an individual tree of a particular species.
#' @param species Column name of the species variable in `data`. Defaults to `species`.
#' @param response Column name of the response variable in `data`. Defaults to
#'   `height`.
#' @param predictor Column name of the predictor variable in `data`. Defaults to
#'   `diameter`.
#' @param length.out Number of new X values to generate for each species. Defaults to 100. Set a higher value for greater resolution at the cost of computational time.
#' @return A new dataset containing the columns species, new X, and whether X is extrapolated for each species.
#' @examples
#' newdat <- generate_newX(urbantrees)
#' newdat

# data("urbantrees")
# data <- urbantrees
# response = "height"
# predictor = "diameter"
# species = "species"

generate_newX <-
  function(data,
           response = "height",
           predictor = "diameter",
           species = "species",
           length.out = 100) {

  y <- data[[response]]
  x <- data[[predictor]]
  sp <- data[[species]]
  y_trans <- log(y)

  # generate new x to simulate y
  x_new <- seq(min(x), max(x), length.out = length.out)

  # x range of each species
  x_sp_range <- as.data.frame(do.call(rbind, tapply(x, sp, range)))
  names(x_sp_range) <- c("x_min", "x_max")
  x_sp_range$sp <- rownames(x_sp_range)

  # compile new dataframe
  newdat <-
    expand.grid(
      sp = unique(data[[species]]),
      x = x_new
    )
  newdat <- merge(newdat, x_sp_range, by = "sp")
  newdat <- newdat[order(newdat$sp, newdat$x), ]
  newdat$extrapolated <-
    ifelse(newdat$x < newdat$x_min | newdat$x > newdat$x_max, "Yes", "No")
  newdat <- newdat[, !names(newdat) %in% c("x_min", "x_max")]

  return(newdat)

}


