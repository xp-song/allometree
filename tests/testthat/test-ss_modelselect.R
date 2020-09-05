context("ss_modelselect functions")

test_that("no data", {
  expect_error(ss_modelselect())
})

test_that("wrong data type error", {
  urbantrees$diameter <- as.character(urbantrees$diameter)
  expect_error(ss_modelselect(urbantrees))

  urbantrees$height <- as.character(urbantrees$height)
  expect_error(ss_modelselect(urbantrees))
})

test_that("missing data warning", {
  nos <- sample(1:nrow(urbantrees), 100, replace=FALSE)

  urbantrees$diameter[nos] <- NA
  expect_message(ss_modelselect(urbantrees))
})
