test_that("fit works", {
  data <- tibble::tibble(id = c(1, 1, 2, 2), x = 1:4, y = 2:5)

  recipe <- recipes::recipe(data, y ~ x + id) %>%
    step_nest(id)

  nested_data <- nest(data, data = -id)

  model <- linear_reg() %>%
    nested()

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    add_model(model)

  fit1 <- fit(model, y ~ x, nested_data)
  fit2 <- fit(wf, data)

  expect_s3_class(fit1, "fitted_nested_models")
  expect_s3_class(fit2, "fitted_nested_workflows")

  nested_recipe <- fit2[[1]]
  if (!is.null(nested_recipe)) {
    expect_s3_class(nested_recipe, recipe)
    expect_equal(nested_recipe %>%
      recipes::prep(data) %>%
      recipes::bake(data), nested_data)
  }
})

test_that("general tests", {
  expect_s3_class(fitted, "nested_workflow_fit")
})
