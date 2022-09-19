## code to prepare `example_nested_tune_results` dataset goes here
set.seed(42)

model <- parsnip::linear_reg(penalty = tune()) %>%
  parsnip::set_engine("glmnet") %>%
  nested()

recipe <- recipes::recipe(example_nested_data, z ~ x + y + id) %>%
  recipes::step_pca(x, y, num_comp = tune::tune()) %>%
  step_nest(-id)

wf <- workflows::workflow() %>%
  workflows::add_recipe(recipe) %>%
  add_nested_model(model)

wfs <- list(wf, wf) %>%
  nested()

resamples <- nested_resamples(example_nested_data,
                              rsample::vfold_cv, recipe, 2)

example_nested_tune_results <- tune_nested(
  wfs, tune::tune_grid, resamples
)

usethis::use_data(example_nested_tune_results, overwrite = TRUE)
