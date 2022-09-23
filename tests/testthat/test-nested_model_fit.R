test_that("nested_model_fit works", {
  model <- parsnip::linear_reg() %>%
    nested()
  
  fit <- fit(model, z ~ ., nested_data)
  
  expect_equal(required_pkgs(fit), required_pkgs(model))
})
