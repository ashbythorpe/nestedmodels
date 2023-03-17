test_that("workflows with nested models work", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("hardhat")
  
  model <- parsnip::linear_reg() %>%
    nested()

  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(id, id2)

  wf <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)

  fit <- fit(wf, example_nested_data)

  expect_equal(
    nrow(predict(fit, example_nested_data)),
    nrow(example_nested_data)
  )
  
  expect_equal(
    nrow(predict(fit, example_nested_data)),
    nrow(example_nested_data)
  )

  baked_data <- hardhat::extract_recipe(fit) %>%
    recipes::bake(example_nested_data)

  baked_data$.nest_id <- NULL

  x <- baked_data[, names(baked_data) != "z"]
  y <- baked_data$z

  expect_error(fit_xy(model, x, y))
})

test_that("Nested models can be tuned", {
  skip_on_cran() # Long test
  skip_if_not_installed("withr")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("workflows")
  skip_if_not_installed("tune")
  withr::local_options(warnPartialMatchArgs = FALSE)

  model <- parsnip::linear_reg(
    penalty = hardhat::tune()
  ) %>%
    parsnip::set_engine("glmnet") %>%
    nested()

  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(id)

  wf <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)

  resamples <- nested_resamples(
    example_nested_data %>% dplyr::group_by(id),
    rsample::vfold_cv(v = 4)
  )

  tuned <- tune::tune_grid(
    wf,
    resamples,
    grid = 5
  )

  best <- tune::select_best(tuned, "rmse")

  final_wf <- tune::finalize_workflow(wf, best)

  fit <- fit(final_wf, example_nested_data)

  suppressWarnings(
    expect_equal(
      nrow(predict(fit, example_nested_data)),
      nrow(example_nested_data)
    )
  )
})
