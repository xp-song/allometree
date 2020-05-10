
<!-- README.md is generated from README.Rmd. Please edit that file -->

# allometree <a href='https://xp-song.github.io/allometree/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/xp-song/allometree/workflows/R-CMD-check/badge.svg)](https://github.com/xp-song/allometree/actions)
[![codecov](https://codecov.io/gh/xp-song/allometree/branch/master/graph/badge.svg?token=HS5Q0TAXTK)](https://codecov.io/gh/xp-song/allometree)
![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
<!-- badges: end -->

## Allometric Scaling of Urban Trees

Develop and use allometric equations relating to the size and structure
of urban trees. Refer to [package
website](https://xp-song.github.io/allometree/) and [prototype web
app](https://xpsong.shinyapps.io/allometree-sg/) for demonstrations of
how the package may be used. The package supplements the following
study:

Song, X. P., Lai, H. R., Wijedasa, L. S., Yee, A. T. K., Tan, P. Y.,
Richards, D. R., Streamlining management practices based on the size
allometry of tropical street trees (in prep).

## Installation

``` r
# Install development version from GitHub
devtools::install_github("xp-song/allometree")
```

## Setup

``` r
library(allometree)
```

## Example

Allometric equations in this package have been used to predict
relationships between parameters related tree size and structure, such
as age, height, trunk diameter, crown height, crown diameter, leaf area,
etc. They are foundational to other models that estimate the benefits
and hazards associated with trees as they mature and grow in size. See
the vignette ‘[Get started with allometree](allometree.html)’ for a full
description of example datasets and allometric equations.

Let’s develop models to predict tree *height* from trunk *diameter* for
five species in our example dataset
`data(urbantrees)`:

<img src="man/figures/urbantrees-1.png" style="display: block; margin: auto;" />

 

One method is to develop allometric models separately for each species,
i.e., [single-species linear models](single-species_models.html). We can
select the best-fit equation for each species in the dataset, or fit
data to specified (i.e. pre-defined) equations, for example, after the
removal of outliers. The example below selects the best-fit equation for
each species in
`urbantrees`:

``` r
results <- sp_modelselect_multi(urbantrees, species = "species", # specify colname of species
                                response = "height", predictor = "diameter") # specify colnames of variables
```

 

We can simulate data across a range of diameter sizes for each species,
and use their respective models to make predictions of tree height. The
simulated data can also be extrapolated beyond the range used to fit the
model:

``` r
predictions <- sp_simulate(ref_table = results$sp_models_info, models = results$sp_models, 
                           extrapolate = c(0,1)) # diameter from 0 to 1 m
```

 

Model predictions can be visualised alongside the original data using
`ggplot2::ggplot()`:

<img src="man/figures/single-species_model_curves-1.png" style="display: block; margin: auto;" />
