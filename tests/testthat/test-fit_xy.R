test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

model <- parsnip::linear_reg(
  penalty = tune()
) %>%
  parsnip::set_engine("glmnet") %>%
  nested()

rlang::is_quosures(model$eng_args)
model
fit(model, z ~ ., example_nested_data %>% tidyr::nest(data = -id))

model$args
hardhat:::extract_parameter_set_dials.model_spec

sloop::s3_methods_generic("extract_parameter_set_dials")
getS3method("extract_parameter_set_dials", "model_spec")
?generics::tune_args

model2 <- parsnip::svm_rbf(
  mode = "regression", 
  cost = tune()
) %>%
  parsnip::set_engine("kernlab") %>%
  nested()

recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
  step_nest(id)
getS3method("update", "linear_reg")
parsnip::update_spec
?update_spec
parsnip::new_model_spec
parsnip::make_classes
update.linear_reg
wf <- workflows::workflow() %>%
  workflows::add_model(model) %>%
  workflows::add_recipe(recipe)

fit(wf, example_nested_data)

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

stacks::fit_members
stacks:::fit_member
blend[["mode"]]
dat <- blend[["train"]]
member_names <- stacks:::.get_glmn_coefs(blend[["coefs"]][["fit"]], 
                                blend[["coefs"]][["spec"]][["args"]][["penalty"]]) %>% 
  dplyr::filter(estimate != 0 & terms != "(Intercept)") %>% 
  dplyr::pull(terms)

members_map <- tibble::enframe(blend[["cols_map"]]) %>% 
  tidyr::unnest(cols = value) %>% dplyr::full_join(metrics_dict, 
                                                   by = c(value = ".config"))
members_map
metrics_dict <- tibble::enframe(blend[["model_metrics"]]) %>% 
  tidyr::unnest(cols = value) %>% dplyr::mutate(.config = stacks:::process_.config(.config, 
                                                                          ., name = make.names(name)))
metrics_dict

new_member <- tune::finalize_workflow(member_wf, member_metrics[,member_params])
new_member %>%
  extract_spec_parsnip()
library(tidymodels)


?tune::finalize_workflow()
tune::finalize_model
tune:::update_model
tune:::update
wflows = blend[["model_defs"]]
member_row <- members_map %>%
  dplyr::filter(value == member_names)
member_params <- wflows[[member_row$name.x[1]]] %>% parsnip::extract_parameter_set_dials() %>% 
  dplyr::pull(id)
members_map
member_metrics <- members_map %>% dplyr::filter(value == 
                                                  member_names) %>% dplyr::slice(1)
member_wf <- wflows[[member_metrics$name.x]]
asNamespace("stacks")$fit_member
member_metrics

?workflowsets::workflow_set
fit <- fit(wf, example_nested_data)
fit
example_nested_data$y <- runif(100)


fit
fit$fit$fit$.model_fit[[1]]
example_nested_data

nested_data <- example_nested_data %>% 
  tidyr::nest(data = -id)
fit <- fit(model, z ~ ., nested_data)
model_fit$fit
predict(model_fit, example_nested_data)
nested_data
any(!"id" %in% colnames(example_nested_data))

nested_data

nested_data$data

predict(fit$fit$.model_fit[[1]], nested_data$data[[1]])
predict(fit, example_nested_data)
example_nested_data
workflows:::predict.workflow

predict(fit, example_nested_data)

resamples <- nested_resamples(
  example_nested_data %>% tidyr::nest(data = -id), 
  rsample::vfold_cv()
)

example_nested_data$a <- seq(1, 1000, length.out = 100)

wf$fit$actions$model$spec %>%
  class()
tunable(wf)
tuned <- tune::tune_grid(
  wf,
  resamples,
  grid = 1
)

best <- tune::select_best(tuned, "rmse")

final_wf <- tune::finalize_workflow(wf, best)

final_wf

fit <- fit(final_wf, example_nested_data)
model
predict(fit, example_nested_data)

tune:::tune_grid_loop_iter_safely
tune:::tune_grid_loop_iter
tune::finalize_model(model, list(penalty = 1))

print.nested_model
linear_reg() %>% unclass()
model
unclass(model)

tune:::tune_grid_workflow
tune:::check_workflow
model$engine
tune:::check_installs
parsnip::get_dependency("nested_model")
hardhat::extract_spec_parsnip(wf)
parsnip:::find_tune_id(model$args$penalty)
parsnip:::find_tune_id

rlang::eval_tidy(model$args$penalty)
update(model, penalty = 1) %>%
  class()
tune:::tune_grid_workflow
tune:::check_parameters

x <- generics::tunable(model)
y <- generics::tune_args(model)
x
parsnip:::eval_call_info()
eval_call_info
x
y
dplyr::inner_join(y %>% dplyr::select(-tunable, 
                                      -component_id), 
                  x, by = c("name", "source", "component")) %>% 
  mutate(object = purrr::map(call_info, parsnip:::eval_call_info))


hardhat::extract_parameter_set_dials
hardhat::extract_parameter_set_dials(model)
hardhat::extract_parameter_set_dials(model2)
getS3method("extract_parameter_set_dials", "model_spec")
generics::tune_args(wf)
parsnip:::tune_args_model_spec(model)
parsnip:::tune_tbl
model2 <- linear_reg(penalty = tune())

parsnip:::find_tune_id(x)
parsnip:::find_tune_id
parsnip:::convert_args
rlang::is_quosures(model$args$penalty)
rlang::is_quosure(model$args$penalty)
rlang::enquo()
parsnip:::tune_id(.x)
x <- rlang::quo_get_expr(model$args$penalty)
parsnip:::tune_id
parsnip:::convert_args

sloop::s3_dispatch(update(model))

showMethods(update)
sloop::s3_methods_generic("update")

update.linear_reg

sloop::s3_get_method(update.linear_reg)

tidy(fit$fit$.model_fit[[1]])
unloadNamespace("stacks")
f_wf <- workflows::workflow() %>%
  workflows::add_model(model) %>%
  workflows::add_formula(z ~ .)

rlang::eval_tidy(
  rlang::call2("multi_predict", .ns = "parsnip", rlang::expr(fit), 
               rlang::expr(example_nested_data))
)
tuned
sloop::s3_methods_generic("multi_predict")
sloop::s3_dispatch(multi_predict(fit, example_nested_data))
fit %>%
  class()
class(model)
fit <- fit(model, z ~ ., example_nested_data %>% tidyr::nest(data = -id))
rlang::exec("multi_predict", fit, example_nested_data, .env = rlang::ns_env("parsnip"))
?rlang::exec
multi_predict(fit, example_nested_data %>% tidyr::nest(data = -id))
fit
tune:::predict_model

fit(f_wf, example_nested_data %>% tidyr::nest(data = -id))


