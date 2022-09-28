test_that("predict works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)
  
  fit <- fit(model, z ~ ., nested_data)
  
  suppressWarnings({
    expect_equal(predict(fit, example_nested_data), 
                 predict(fit, nested_data))
    
    expect_equal(predict(fit, example_nested_data %>%
                           dplyr::filter(id == 10)),
                 predict(fit, example_nested_data)[
                   which(example_nested_data$id == 10),])
  })
})

test_that("predict_raw.nested_model_spec works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)
  
  fit <- fit(model, z ~ ., nested_data)
  
  suppressWarnings(
    expect_equal(length(predict_raw(fit, example_nested_data)), 
                 nrow(example_nested_data))
  )
})

test_that("multivariate predictions work", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)
  
  fit <- fit(model, cbind(z, y) ~ ., nested_data)
  
  expect_equal(ncol(predict(fit, example_nested_data)), 2)
  predict_raw(fit, example_nested_data)
  augment(fit, example_nested_data)
})

test_that("multivariate workflows work", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()
  
  recipe <- recipes::recipe(example_nested_data, z + y ~ .) %>%
    step_nest(id)
  
  wf <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)
  
  fit <- fit(wf, example_nested_data)
  
  expect_equal(ncol(predict(fit, example_nested_data)), 2)
  augment(fit, example_nested_data)
})
