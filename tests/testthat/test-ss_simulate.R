context("ss_simulate")


test_that("Check data type", {

  result <- ss_modelselect(urbantrees)

  expect_error(
    ss_simulate(ref_table = result$ss_models_info, models = result$ss_models, extrapolate = c(-1,0.3)) # extrapoate down to negative
    )

})
