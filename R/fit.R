#' Fit a nested model or workflow to a dataset
#'
#' [generics::fit()] methods for nested models and workflows.
#' Do not use `fit_xy` for nested models or workflows.
#'
#' @param object An object of class 'nested_model', 'nested_workflow', 
#' 'nested_models' or 'nested_workflows'.
#' @param formula An object of class formula. Passed into 
#' [parsnip::fit.model_spec()].
#' @param data A data frame. If used with a 'nested_model' object, the data 
#' frame must already be nested.
#' @param control A [parsnip::control_parsnip()], 
#' [workflows::control_workflow()] or [control_nested()] object.
#' @param ... Passed into the respective fit functions. Currently unused.
#'
#' @returns A 'nested_model_fit' or 'nested_workflow_fit' object.
#'
#' @seealso [parsnip::fit.model_spec] [workflows::fit.workflow]
#'
#' @examples
#' data("example_nested_data")
#'
#' model <- parsnip::linear_reg() %>%
#'   parsnip::set_engine("lm") %>%
#'   nested()
#'
#' recipe <- recipes::recipe(example_nested_data, z ~ x + y + id) %>%
#'   step_nest(id)
#'
#' wf <- workflow() %>%
#'   add_recipe(recipe) %>%
#'   add_model(model)
#'
#' fit(wf, example_nested_data)
#'
#' @importFrom generics fit
#'
#' @export
fit.nested_model <- function(object, formula, data, case_weights = NULL,
                             control = parsnip::control_parsnip(), ...) {
  model <- object$eng_args$model_spec[[1]]
  
  if(!is.null(object$args)) {
    model <- pass_down_args(model, object)
  }
  
  nested_data <- nest_data_method(data)

  fits <- purrr::map(nested_data$data, fit, object = model,
                     formula = formula, case_weights = case_weights, 
                     control = control, ...)
  
  cols <- colnames(purrr::compact(nested_data$data)[[1]])
  
  nested_data %>%
    dplyr::select(-.data$data) %>%
    dplyr::mutate(.model_fit = .env$fits) %>%
    new_nested_model_fit(spec = model, inner_names = cols)
}
