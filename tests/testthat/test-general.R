
# A lot of tests are done in the same test_that function to avoid doing the same thing multiple times
test_that("general_tests", {
  data <- example_nested_data
  
  model <- parsnip::linear_reg(penalty = tune()) %>%
    parsnip::set_engine("glmnet") %>%
    nested()
  
  expect_s3_class(model, c("linear_reg", "nested_model", "model_spec"))
  expect_true(is_nested(model))

  recipe <- recipes::recipe(data, z ~ x + y + id) %>%
    recipes::step_pca(x, y, num_comp = tune::tune()) %>%
    step_nest(-id)
  
  tidy(recipe)
  
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    add_nested_model(model)

  expect_s3_class(wf, c("nested_workflow", "workflow"))

  resamples <- nested_resamples(data, rsample::vfold_cv, recipe, 2)
  
  expect_s3_class(resamples, "nested_resamples")

  # since 2 fold cv
  expect_equal(nrow(tidy(resamples)), nrow(data) * 2)

  tuned_1 <- tune_nested(
    object = wf, .f = tune::tune_grid, resamples = resamples,
    control = tune::control_grid(save_pred = T)
  )
  augment(tuned_1)
  
  suppressWarnings({
    tune::show_best(tuned_1)
    tune::select_by_pct_loss(tuned_1, penalty)
    tune::select_by_one_std_err(tuned_1, penalty)
  })

  best <- tuned_1 %>%
    tune::select_best("rmse")

  finalize_model(model, best)
  nested_data <- finalize_recipe(recipe, best) %>%
    prep() %>%
    bake(NULL)

  expect_equal(nrow(nested_data), 2)

  final_workflow <- finalize_workflow(wf, best)

  expect_s3_class(final_workflow, "nested_workflow")

  fitted <- fit(final_workflow, data)

  fit_resamples(final_workflow, resamples)
  initial_split <- nested_resamples(data, rsample::initial_split, recipe)
  last_fit(final_workflow, initial_split)

  getS3method("last_fit", "workflow")

  expect_s3_class(fitted, "nested_workflow_fit")
  
  tidy(fitted)
  augment(fitted, data)
  expect_equal(nrow(glance(fitted)), 1)
  expect_equal(nrow(predict(fitted, data)), nrow(data))
})
