test_that("nested_resamples works", {
  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(id)
  
  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe)
  
  nested_data <- tidyr::nest()
})
