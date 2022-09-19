test_that("nested works", {
  x <- linear_reg() %>%
    set_engine("glmnet") %>%
    nested()
  expect_equal(class(x), c("linear_reg", "nested_model", "model_spec"))
})

test_that("control_nested is saved", {
  x <- linear_reg() %>%
    set_engine("glmnet") %>%
    nested(control = control_nested(verbose = T))
  expect_equal(attributes(x)$control_nested, control_nested(verbose = T))
})

test_that("general tests", {
  expect_s3_class(model, c("linear_reg", "nested_model", "model_spec"))
  expect_true(is_nested(model))
})
