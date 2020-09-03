# Script to run mixed-effect models when fitting multiple species at once

require(lme4)
require(MuMIn)

# NOT RUN: testing only
# load(file = "data/urbantrees.rda")
# data <- subset(urbantrees, height < 40)
# response = "height"
# predictor = "diameter"
# species = "species"

# Calculate geometric mean height
geom_mean_y <- exp(mean(log(data[[response]])))

# extract values to use in model
y <- data[[response]]
x <- data[[predictor]]
y_trans <- log(y)
y_trans_adj <- y_trans * geom_mean_y
sp <- data[[species]]

# formula list
mlm.formula.list <- list(
  lin = as.formula("y ~ 1 + x + (1 + x | sp)"),
  quad = as.formula("y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | sp)"),
  cub = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + (1 + x + I(x^2) + I(x^3) | sp)"),
  quart = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + (1 + x + I(x^2) + I(x^3) + I(x^4) | sp)"),
  loglog = as.formula("y_trans_adj ~ 1 + I(log(log(x+1))) + (1 + I(log(log(x+1))) | sp)"),
  expo = as.formula("y_trans_adj ~ 1 + x + (1 + x | sp)")
)

# weights
weight.list <- list(
  w1 = NULL,
  w2 = sqrt(x),
  w3 = x,
  w4 = x^2
)

# fit mixed models in a loop
# consider refarctoring this to lapply or parallelisation in the future
# m <-
#   lmer(
#   formula = mlm.formula.list$loglog,
#   weights = weight.list$w1,
#   control = lme4::lmerControl(optimizer = "bobyqa")
# )

mlm.list <- list()
i <- 1
for (f in seq_len(length(mlm.formula.list))) {
  for (w in seq_len(length(weight.list))) {
    mlm.list[[i]] <-
      lmer(
        formula = mlm.formula.list[[f]],
        weights = weight.list[[w]],
        control = lme4::lmerControl(optimizer = "bobyqa")
      )
    i <- i + 1
  }
}
names(mlm.list) <-
  as.vector(t(outer(names(mlm.formula.list), names(weight.list), paste, sep = "_")))

# print convergence and warning messages
lapply(mlm.list, function(m) m@optinfo$conv$lme4$messages)

# Model comparison
model.sel(mlm.list)
