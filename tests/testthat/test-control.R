test_that("control_nested_model works", {
  ctrl <- control_nested_model()
  expect_s3_class(ctrl, "control_nested_model")
  expect_equal(print(ctrl), ctrl)
})

test_that("control_workflow works", {
  ctrl <- control_nested_workflow()
  expect_s3_class(ctrl, "control_nested_workflow")
  expect_equal(print(ctrl), ctrl)
})

test_that("errors are correct", {
  expect_error(control_nested_model(verbose = 1))
  expect_error(control_nested_model(verbose = c(T, F)))
  expect_error(control_nested_model(allow_par = 1))
  expect_error(control_nested_model(allow_par = c(T, F)))
  expect_error(control_nested_model(control = 1))
  expect_error(control_nested_workflow(verbose = 1))
  expect_error(control_nested_workflow(verbose = c(T, F)))
  expect_error(control_nested_workflow(allow_par = 1))
  expect_error(control_nested_workflow(allow_par = c(T, F)))
  expect_error(control_nested_workflow(control = 1))
})
