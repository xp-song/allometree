

# library(MuMIn) #for AICc library(nlme)

# function
sp_modelchooser <- function(species, dataset) {
    
    # create empty df
    models <- data.frame(species = character(), model = character())
    models[0:length(species), ] <- NA
    models$species <- as.character(models$species)
    models$model <- as.character(models$model)
    
    for (i in 1:length(species)) {
        
        traind <- dataset[which(dataset$species == species[i]), ]  #select species data
        
        # Calc comparable AICc for loglog / exp models (McPherson et al. 2016) mean_y <- mean(traind$height2) #arithmetic
        # mean
        geom_mean_y <- exp(mean(log(traind$height2)))  #geometric mean PER SPECIES
        traind$height3 <- log(traind$height2) * geom_mean_y  #according to McPherson et al 2016
        
        # fit models
        lin <- lm(height2 ~ diameter, data = traind)
        quad <- lm(height2 ~ diameter + I(diameter^2), data = traind)
        cub <- lm(height2 ~ diameter + I(diameter^2) + I(diameter^3), data = traind)
        quart <- lm(height2 ~ diameter + I(diameter^2) + I(diameter^3) + I(diameter^4), data = traind)
        
        # use height3
        loglogw1 <- lm(height3 ~ I(log(log(diameter + 1))), data = traind)
        loglogw2 <- lm(height3 ~ I(log(log(diameter + 1))), data = traind, weights = I(1/sqrt(traind$diameter)))
        loglogw3 <- lm(height3 ~ I(log(log(diameter + 1))), data = traind, weights = I(1/traind$diameter))
        loglogw4 <- lm(height3 ~ I(log(log(diameter + 1))), data = traind, weights = I(1/traind$diameter^2))
        
        expow1 <- lm(height3 ~ diameter, data = traind)
        expow2 <- lm(height3 ~ diameter, data = traind, weights = I(1/sqrt(traind$diameter)))
        expow3 <- lm(height3 ~ diameter, data = traind, weights = I(1/traind$diameter))
        expow4 <- lm(height3 ~ diameter, data = traind, weights = I(1/traind$diameter^2))
        
        aic <- AICc(lin, quad, cub, quart, loglogw1, loglogw2, loglogw3, loglogw4, expow1, expow2, expow3, expow4)
        
        models$species[i] <- species[i]
        models$model[i] <- rownames(aic)[order(aic$AICc)][1]  #name of model w lowest AICc
        
    }
    
    ### ADD DETAILS AFTER CHOOSING MODEL###
    
    # add cols to models df
    models$a <- as.numeric(NA)
    models$b <- as.numeric(NA)
    models$height.geomean <- as.numeric(NA)
    models$correctn.fctr <- as.numeric(NA)
    models$Residual.SE <- as.numeric(NA)
    models$Mean.SE <- as.numeric(NA)
    models$adj.R2 <- as.numeric(NA)
    models$n <- as.integer(NA)
    
    # empty list for model object
    modellist <- list()
    
    # loop to fill in info for each species:
    for (i in 1:nrow(models)) {
        traind <- dataset[which(dataset$species == models$species[i]), ]  #select species data
        geom_mean_y <- exp(mean(log(traind$height2)))  #geometric mean PER SPECIES
        traind$height3 <- log(traind$height2) * geom_mean_y  #according to McPherson et al 2016
        
        if (models$model[i] == "lin") {
            modelobj <- lm(height2 ~ diameter, data = traind)
        } else if (models$model[i] == "quad") {
            modelobj <- lm(height2 ~ diameter + I(diameter^2), data = traind)
        } else if (models$model[i] == "cub") {
            modelobj <- lm(height2 ~ diameter + I(diameter^2) + I(diameter^3), data = traind)
        } else if (models$model[i] == "quart") {
            modelobj <- lm(height2 ~ diameter + I(diameter^2) + I(diameter^3) + I(diameter^4), data = traind)
        } else if (models$model[i] == "loglogw1") {
            modelobj <- lm(height3 ~ I(log(log(diameter + 1))), data = traind)
        } else if (models$model[i] == "loglogw2") {
            modelobj <- lm(height3 ~ I(log(log(diameter + 1))), data = traind, weights = 1/sqrt(diameter))
        } else if (models$model[i] == "loglogw3") {
            modelobj <- lm(height3 ~ I(log(log(diameter + 1))), data = traind, weights = 1/diameter)
        } else if (models$model[i] == "loglogw4") {
            modelobj <- lm(height3 ~ I(log(log(diameter + 1))), data = traind, weights = 1/I(diameter^2))
        } else if (models$model[i] == "expow1") {
            modelobj <- lm(height3 ~ diameter, data = traind)
        } else if (models$model[i] == "expow2") {
            modelobj <- lm(height3 ~ diameter, data = traind, weights = 1/sqrt(diameter))
        } else if (models$model[i] == "expow3") {
            modelobj <- lm(height3 ~ diameter, data = traind, weights = 1/diameter)
        } else if (models$model[i] == "expow4") {
            modelobj <- lm(height3 ~ diameter, data = traind, weights = 1/I(diameter^2))
        }
        
        models$a[i] <- summary(modelobj)$coef[, "Estimate"][[1]]  #intercept
        models$b[i] <- summary(modelobj)$coef[, "Estimate"][[2]]  #b
        
        # include other parameters (print NA if absent)
        models$c[i] <- tryCatch(summary(modelobj)$coef[, "Estimate"][[3]], error = function(e) NA)  #c
        models$d[i] <- tryCatch(summary(modelobj)$coef[, "Estimate"][[4]], error = function(e) NA)  #d
        models$e[i] <- tryCatch(summary(modelobj)$coef[, "Estimate"][[5]], error = function(e) NA)  #e
        
        models$height.geomean[i] <- geom_mean_y
        models$Residual.SE[i] <- round(summary(modelobj)$sigma, 4)
        models$Mean.SE[i] <- round(mean(residuals(modelobj)^2), 4)  #impt for back-transform according to Peper et al. (2001) #Also can use sjstats::mse(sp_modellist[[i]])
        
        models$adj.R2[i] <- round(summary(modelobj)$adj.r.squared, 4)  #R2
        # models$F.statistic <- summary(modelobj)$fstatistic[[1]]
        
        models$n[i] <- nrow(traind)  #no of data points
        
        
        modellist[[i]] <- modelobj  #add model to the empty list
        names(modellist)[i] <- models$species[i]
    }
    
    
    # correction factor for each single-species model
    for (i in seq_len(length(modellist))) {
        if ("height3" %in% names(modellist[[i]]$model)) {
            # for log-transformed or exp models
            sp_rmse <- sjstats::mse(modellist[[i]])/models$height.geomean[i]
            models$correctn.fctr[i] <- exp((sp_rmse^2)/2)
        } else {
            models$correctn.fctr[i] <- 1
        }
    }
    
    # reorder columns
    models <- models[, c("species", "model", "a", "b", "c", "d", "e", "height.geomean", "correctn.fctr", "Residual.SE", 
        "Mean.SE", "adj.R2", "n")]
    
    return(list(models, modellist))
}
