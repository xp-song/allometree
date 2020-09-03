# Script to run mixed-effect models when fitting multiple species at once

require(lme4)
require(MuMIn)
require(sjstats)

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
# for model selection
mlm.formula.list <- list(
  lin = as.formula("y ~ 1 + x + (1 + x | sp)"),
  quad = as.formula("y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | sp)"),
  cub = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + (1 + x + I(x^2) + I(x^3) | sp)"),
  quart = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + (1 + x + I(x^2) + I(x^3) + I(x^4) | sp)"),
  loglog = as.formula("y_trans_adj ~ 1 + I(log(log(x+1))) + (1 + I(log(log(x+1))) | sp)"),
  expo = as.formula("y_trans_adj ~ 1 + x + (1 + x | sp)")
)
# for refitting the best model
refit.formula.list <- list(
  lin = as.formula("y ~ 1 + x + (1 + x | sp)"),
  quad = as.formula("y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | sp)"),
  cub = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + (1 + x + I(x^2) + I(x^3) | sp)"),
  quart = as.formula("y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + (1 + x + I(x^2) + I(x^3) + I(x^4) | sp)"),
  loglog = as.formula("y_trans ~ 1 + I(log(log(x+1))) + (1 + I(log(log(x+1))) | sp)"),
  expo = as.formula("y_trans ~ 1 + x + (1 + x | sp)")
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
mlm.list <- list()
i <- 1
for (f in seq_len(length(mlm.formula.list))) {
  for (w in seq_len(length(weight.list))) {
    message(paste0("Fitting ", names(mlm.formula.list)[f], " model with weight ", names(weight.list)[w]))
    mlm.list[[i]] <-
      lmer(
        formula = mlm.formula.list[[f]],
        weights = weight.list[[w]],
        control = lmerControl(optimizer = "bobyqa",
                              optCtrl = list(maxfun = 10000)),
        REML = FALSE
      )
    i <- i + 1
  }
}
names(mlm.list) <-
  as.vector(t(outer(names(mlm.formula.list), names(weight.list), paste, sep = "_")))

# print convergence and warning messages
mlm_msg <- lapply(mlm.list, function(m) m@optinfo$conv$lme4$messages)

# Model selection
mlm_comp <- model.sel(mlm.list)
mlm_comp_cols_print <- c("df", "logLik", "AICc", "delta")

# Best model
best_mod_name <- rownames(mlm_comp)[1]
best_mod_formula <- strsplit(best_mod_name, "_")[[1]][1]
best_mod_weight <- strsplit(best_mod_name, "_")[[1]][2]
# mlm_best <- get.models(mlm_comp, subset = 1)[[1]]
# refit the best model using REML, and
# change the response if it is a loglog or expo model
best_mod_refit <-
  lmer(
    formula = refit.formula.list[[best_mod_formula]],
    weights = weight.list[[best_mod_weight]],
    control = lmerControl(optimizer = "bobyqa",
                          optCtrl = list(maxfun = 10000)),
    REML = TRUE
  )

# Species-specific parameters and information
# Parameters
params <- coef(best_mod_refit)$sp
colnames(params) <- letters[1:ncol(params)]
# Correction factor
if (best_mod_formula %in% c("loglog", "expo")) {
  # for transformed (loglog or exp) models
  sp_rmse <- rmse(best_mod_refit)
  cf <- exp((sp_rmse^2)/2)
  params[, "a"] <- params[, "a"] + log(cf)  #directly adjust intercept with cf
} else {
  # for non-transformed models
  cf <- 1
}
sp_info <-
  data.frame(species = rownames(sp_info),
             modelcode = best_mod_name,
             params,
             correctn_factor = cf,
             row.names = NULL)

# Compile outputs
out <- list()
out$models_rank <- mlm_comp[, mlm_comp_cols_print]
out$best_model <- best_mod_refit
out$R2 <- r.squaredGLMM(best_mod_refit)

