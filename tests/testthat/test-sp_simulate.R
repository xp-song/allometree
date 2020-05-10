context("sp_simulate")


test_that("Check data type", {

  result <- sp_modelselect(urbantrees)

  expect_error(
    sp_simulate(ref_table = result$sp_models_info, models = result$sp_models, extrapolate = c(-1,0.3)) # extrapoate down to negative
    )

})
