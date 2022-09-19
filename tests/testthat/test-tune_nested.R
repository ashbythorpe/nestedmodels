test_that("general tests", {
  tune::show_best(tuned_1, "rmse")
  suppressWarnings({
    tune::select_by_pct_loss(tuned_1, dplyr::desc(penalty), metric = "rsq")
    tune::select_by_one_std_err(tuned_1, dplyr::desc(penalty), metric = "rsq")
  })
  finalize_model(model, best)
  
  nested_data <- finalize_recipe(recipe, best) %>%
    prep() %>%
    bake(NULL)
  
  expect_equal(nrow(nested_data), 2)
  
  expect_s3_class(final_workflow, "nested_workflow")
})

