#' Fit a nested model to a dataset
#'
#' `fit.model_spec()` takes a nested model specification and fits the inner
#' model specification to each nested data frame in the given dataset.
#'
#' @param object An `nested_model` object (see [nested()]).
#' @param formula An object of class `formula`. Passed into
#'   [parsnip::fit.model_spec()]. This should *not contain* the variable to
#'   nest by.
#' @param data A data frame. If used with a 'nested_model' object, the data
#'   frame must already be nested.
#' @param case_weights An optional vector of case weights. Passed into
#'   [parsnip::fit.model_spec()].
#' @param control A [parsnip::control_parsnip()] object. Passed into
#'   [parsnip::fit.model_spec()].
#' @param ... Passed into [parsnip::fit.model_spec()]. Currently unused.
#'
#' @returns A `nested_model_fit` object with several elements:
#' * `spec`: The model specification object (the inner model of the
#'   nested model object)
#' * `fit`: A tibble containing the model fits and the nests that they
#'   correspond to.
#' * `inner_names`: A character vector of names, used to help with
#'   nesting the data during predictions.
#'
#' @seealso [parsnip::fit.model_spec()] [parsnip::model_fit]
#'
#' @examples
#' 
#' library(parsnip)
#' library(tidyr)
#' 
#' model <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   nested()
#'
#' nested_data <- nest(example_nested_data, data = -id)
#'
#' fit(model, z ~ x + y + a + b, nested_data)
#'
#' @export
fit.nested_model <- function(object, formula, data, case_weights = NULL,
                             control = parsnip::control_parsnip(), ...) {
  data <- check_df(data, "data")

  model <- extract_inner_model(object)
  parallel <- object$eng_args$allow_par
  pkgs <- object$eng_args$pkgs
  
  if (!is.null(object$args)) {
    model <- pass_down_args(model, object)
  }

  nest_data_results <- nest_data_method(data)
  nested_data <- nest_data_results$data
  nested_colname <- nest_data_results$colname
  
  `%op%` <- get_operator(parallel, model)
  
  rlang::local_options(doFuture.rng.onMisuse = "ignore")
  
  fits <- foreach::foreach(
    data = nested_data[[nested_colname]],
    .export = "safe_fit",
    .packages = unique(c(pkgs, generics::required_pkgs(model)))
  ) %op% {
    safe_fit(model, formula, data, case_weights = case_weights, 
             control = control, ...)
  }

  cols <- colnames(purrr::compact(nested_data[[nested_colname]])[[1]])

  fit <- nested_data[, names(nested_data) != nested_colname]
  fit$.model_fit <- fits

  new_nested_model_fit(fit = fit, spec = model, inner_names = cols)
}

#' @noRd
safe_fit <- function(...) {
  try(fit(...))
}
