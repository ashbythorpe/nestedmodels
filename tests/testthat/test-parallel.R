test_that("Fitting works in parallel", {
  skip_if_not_installed("withr")
  skip_if_not_installed("parallel")
  skip_if_not_installed("doParallel")
  
  withr::defer({
    doParallel::stopImplicitCluster()
    foreach::registerDoSEQ()
  })
  
  foreach::registerDoSEQ()
  
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested(allow_par = TRUE)
  
  expect_false(allow_parallelism(model$eng_args$allow_par, model))
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)
  
  fit_1 <- fit(model, z ~ x + y + a + b, nested_data)
  
  preds_1 <- predict(fit_1, example_nested_data)
  
  cl <- parallel::makePSOCKcluster(2)
  doParallel::registerDoParallel(cl)
  
  expect_true(allow_parallelism(model$eng_args$allow_par, model))
  
  fit_2 <- fit(model, z ~ x + y + a + b, nested_data)
  
  preds_2 <- predict(fit_2, example_nested_data)
  
  expect_equal(preds_1, preds_2)
  
  parallel::stopCluster(cl)
})

test_that("Fitting workflows works in parallel", {
  skip_if_not_installed("withr")
  skip_if_not_installed("parallel")
  skip_if_not_installed("doParallel")
  skip_if_not_installed("workflows")
  
  withr::defer({
    doParallel::stopImplicitCluster()
    foreach::registerDoSEQ()
  })
  
  foreach::registerDoSEQ()
  
  model <- parsnip::linear_reg() %>%
    nested(allow_par = TRUE)
  
  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(id, id2)
  
  wf <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)
  
  fit_1 <- fit(wf, example_nested_data)
  
  preds_1 <- predict(fit_1, example_nested_data)
  
  cl <- parallel::makePSOCKcluster(2)
  doParallel::registerDoParallel(cl)
  
  fit_2 <- fit(wf, example_nested_data)
  
  preds_2 <- predict(fit_2, example_nested_data)
  
  expect_equal(preds_1, preds_2)
  
  parallel::stopCluster(cl)
})
