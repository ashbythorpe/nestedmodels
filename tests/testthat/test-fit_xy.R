test_that("workflows work", {
  model <- parsnip::linear_reg() %>%
    nested()
  
  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(id, id2)
  
  wf <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)
  
  fit <- fit(wf, example_nested_data)
  
  suppressWarnings({
    expect_equal(nrow(predict(fit, example_nested_data)), 
                 nrow(example_nested_data))
    expect_equal(nrow(predict(fit, example_nested_data)), 
                 nrow(example_nested_data))
  })
})

test_that("Nested models can be tuned", {
  skip_on_cran() # Long test
  skip_if_not_installed("glmnet")
  skip_if_not_installed("tune")
  
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
    grid = 5,
    control = tune::control_grid(verbose = T)
  )
  
  best <- tune::select_best(tuned, "rmse")
  
  final_wf <- tune::finalize_workflow(wf, best)
  
  fit <- fit(final_wf, example_nested_data)
  
  suppressWarnings(predict(fit, example_nested_data))
})

simple_model <- parsnip::linear_reg() %>%
  nested()

recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
  step_nest(id, id2)

simple_wf <- workflows::workflow() %>%
  workflows::add_model(simple_model) %>%
  workflows::add_recipe(recipe)

model <- parsnip::linear_reg(
  penalty = tune()
) %>%
  parsnip::set_engine("glmnet") %>%
  nested()

model

model2 <- parsnip::svm_rbf(
  mode = "regression", 
  cost = tune()
) %>%
  parsnip::set_engine("kernlab") %>%
  nested()

wf <- workflows::workflow() %>%
  workflows::add_model(model) %>%
  workflows::add_recipe(recipe)

wfset <- workflowsets::workflow_set(list(recipe), list(model, model2))

wfset

tuned_wfset <- workflowsets::workflow_map(
  wfset,
  "tune_grid",
  resamples = resamples,
  control = tune::control_grid(
    save_pred = T,
    save_workflow = T,
    verbose = T
  )
)

tuned_wfset

workflowsets::rank_results(tuned_wfset, select_best = T)

stack <- stacks::stacks() %>%
  stacks::add_candidates(tuned_wfset)

blend <- stacks::blend_predictions(stack)

stack_fit <- stacks::fit_members(blend)

predict(stack_fit, example_nested_data)


nested_data <- example_nested_data %>% 
  tidyr::nest(data = -id)
nested_data
system.time({
  model_fit <- fit(simple_model, z ~ ., nested_data)
})
model_fit <- fit(simple_model, z ~ ., nested_data)

print(model_fit)

tidy()

class(model_fit$fit)

predict(model_fit, example_nested_data)

augment(model_fit, example_nested_data)

wf_fit <- fit(simple_wf, example_nested_data)

predict(wf_fit, example_nested_data)

augment(wf_fit, example_nested_data)

resamples <- nested_resamples(
  example_nested_data %>% dplyr::group_by(id), 
  rsample::vfold_cv()
)

tuned <- tune::tune_grid(
  wf,
  resamples,
  grid = 1
)

best <- tune::select_best(tuned, "rmse")

final_wf <- tune::finalize_workflow(wf, best)

final_wf

fit <- fit(final_wf, example_nested_data)

predict(fit, example_nested_data)

