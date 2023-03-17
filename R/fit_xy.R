#' Fit a nested model to a dataset using an xy interface.
#'
#' [generics::fit_xy()] method for nested models. This should not be
#' called directly and instead should be called by
#' [workflows::fit.workflow()].
#'
#' @param object An `nested_model` object (see [nested()]).
#' @param x A data frame of predictors.
#' @param y A data frame of outcome data.
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
#' @examplesIf rlang::is_installed("workflows")
#' 
#' library(dplyr)
#' library(parsnip)
#' library(recipes)
#' library(workflows)
#' 
#' data <- filter(example_nested_data, id %in% 11:20)
#'
#' model <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   nested()
#'
#' recipe <- recipe(data, z ~ x + y + id) %>%
#'   step_nest(id)
#'
#' wf <- workflow() %>%
#'   add_recipe(recipe) %>%
#'   add_model(model)
#'
#' fit(wf, data)
#'
#' @importFrom generics fit_xy
#'
#' @export fit_xy.nested_model
#' @export
fit_xy.nested_model <- function(object, x, y, case_weights = NULL,
                                control = parsnip::control_parsnip(), ...) {
  x <- check_df(x, "x")
  if (is.vector(y)) {
    y <- data.frame(outcome = y)
  } else {
    y <- check_df(y, "y")
  }

  model <- extract_inner_model(object)
  parallel <- object$eng_args$allow_par
  pkgs <- object$eng_args$pkgs
  
  if (!is.null(object$args)) {
    model <- pass_down_args(model, object)
  }

  if (!".nest_id" %in% colnames(x)) {
    cli::cli_abort(c(
      "{.arg x} does not contain a \".nest_id\" column.",
      "i" = "Try using {.fun step_nest}."
    ))
  }

  y$.nest_id <- x$.nest_id

  nested_colname <- get_name(".data", c(colnames(x), colnames(y)))

  nested_x <- x %>%
    tidyr::nest(!!nested_colname := -".nest_id") %>%
    dplyr::ungroup()

  nested_y <- y %>%
    tidyr::nest(!!nested_colname := -".nest_id") %>%
    dplyr::ungroup()
  
  `%op%` <- get_operator(parallel, model)
  
  rlang::local_options(doFuture.rng.onMisuse = "ignore")
  
  fits <- foreach::foreach(
    x = nested_x[[nested_colname]],
    y = nested_y[[nested_colname]],
    .export = "safe_fit_xy",
    .packages = unique(c(pkgs, generics::required_pkgs(model)))
  ) %op% {
    safe_fit_xy(model, x, y, case_weights = case_weights, control = control, ...)
  }

  cols <- c(
    colnames(purrr::compact(nested_x[[nested_colname]])[[1]]),
    colnames(purrr::compact(nested_y[[nested_colname]])[[1]])
  )

  fit <- nested_x[, names(nested_x) != nested_colname]
  fit$.model_fit <- fits

  new_nested_model_fit(spec = model, fit = fit, inner_names = cols)
}

#' @noRd
safe_fit_xy <- function(...) {
  try(fit_xy(...))
}
