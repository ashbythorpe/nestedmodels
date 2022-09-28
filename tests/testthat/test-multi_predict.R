test_that("multi_predict.nested_model_fit works", {
  skip_if_not_installed("glmnet")
  
  model <- parsnip::linear_reg(penalty = hardhat::tune()) %>%
    parsnip::set_engine("glmnet") %>%
    nested()
  
  fit <- fit(model, z ~ ., tidyr::nest(example_nested_data, data = -.data$id))
  
  suppressWarnings(
    pred <- multi_predict(fit, example_nested_data, penalty = c(1,0.5,0.2))
  )
  
  purrr::map_int(pred$.pred, nrow) %>%
    unique() %>%
    expect_equal(3)
})
