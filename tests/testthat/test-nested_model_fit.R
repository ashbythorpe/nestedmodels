test_that("nested_model_fit works", {
  model <- parsnip::linear_reg() %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -.data$id)

  fit <- fit(model, z ~ x + y + a + b, nested_data)

  expect_equal(required_pkgs(fit), required_pkgs(model))

  purrr::quietly(print)(fit)$output %>%
    expect_match("Nested model fit, with 20 inner models")
})

test_that("new_nested_model_fit works", {
  x <- new_nested_model_fit(fit = 1, spec = 2, inner_names = 3)
  expect_s3_class(x, "nested_model_fit")
  expect_equal(unclass(x), list(spec = 2, fit = 1, inner_names = 3))
})
