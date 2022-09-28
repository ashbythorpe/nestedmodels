test_that("fit.nested_model works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)
  
  fit <- fit(model, z ~ ., nested_data)
  
  expect_s3_class(fit, "nested_model_fit")
  expect_equal(fit$inner_names, colnames(nested_data$data[[1]]))
  expect_equal(fit$spec, parsnip::linear_reg() %>%
                 parsnip::set_engine("lm"))
  expect_equal(nrow(fit$fit), length(unique(example_nested_data$id)))
})
