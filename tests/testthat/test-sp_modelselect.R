context("sp_modelselect functions")

test_that("no data", {
  expect_error(sp_modelselect())
})

test_that("wrong data type error", {
  urbantrees$diameter <- as.character(urbantrees$diameter)
  expect_error(sp_modelselect(urbantrees))

  urbantrees$height <- as.character(urbantrees$height)
  expect_error(sp_modelselect(urbantrees))
})

test_that("missing data warning", {
  nos <- sample(1:nrow(urbantrees), 100, replace=FALSE)

  urbantrees$diameter[nos] <- NA
  expect_message(sp_modelselect(urbantrees))
})
