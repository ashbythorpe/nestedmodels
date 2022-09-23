test_that("new_nested_model_fit works", {
  x <- new_nested_model_fit(fit = 1, spec = 2, inner_names = 3)
  expect_s3_class(x, "nested_model_fit")
  expect_equal(unclass(x), list(spec = 2, fit = 1, inner_names = 3))
})
