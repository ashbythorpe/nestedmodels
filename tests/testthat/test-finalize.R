test_that("finalize works", {
  example_best <- tibble::tibble(penalty = 0.2, trim = 1)
  example_best_multiple <- tibble::tibble(nest_id = c(1:10), penalty = 0.2, trim = 1)
  data <- tibble::tibble(x = 1, y = 2)

  model <- parsnip::linear_reg(penalty = tune::tune()) %>%
    parsnip::set_engine("glmnet") %>%
    nested()

  recipe <- recipe(data, y ~ x) %>%
    recipes::step_impute_mean(x, trim = tune())

  wf <- workflows::workflow() %>%
    add_model(model) %>%
    workflows::add_recipe(recipe)

  model %>%
    finalize_model(example_best)

  model %>%
    finalize_model(example_best_multiple)

  recipe %>%
    finalize_recipe(example_best)

  recipe %>%
    finalize_recipe(example_best_multiple)

  workflow %>%
    finalize_workflow(example_best)

  workflow %>%
    finalize_workflow(example_best_multiple)
})
