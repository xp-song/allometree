context("sp_predict")


test_that("wrong data type error", {

  result <- sp_modelselect(urbantrees)

  expect_error(
    sp_simulate(ref_table = result$sp_models_info, models = result$sp_models, extrapolate = c(-1,0.3))
    )

})
