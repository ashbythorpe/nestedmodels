#' Nested model predictions across many sub-models
#'
#' [parsnip::multi_predict()] method for nested models. Allows predictions
#' to be made on sub-models in a model object.
#'
#' @param object A `nested_model_fit` object produced by
#'  [fit.nested_model()].
#' @param new_data A data frame - can be nested or non-nested.
#' @param ... Passed onto [parsnip::multi_predict()]
#'
#' @returns A tibble with the same number of rows as `new_data`, after it
#' has been unnested.
#'
#' @seealso [parsnip::multi_predict()]
#'
#' @examplesIf rlang::is_installed("glmnet")
#' 
#' library(dplyr)
#' library(tidyr)
#' library(parsnip)
#' library(glmnet)
#' 
#' data <- filter(example_nested_data, id %in% 16:20)
#'
#' nested_data <- nest(data, data = -id2)
#'
#' model <- linear_reg(penalty = 1) %>%
#'   set_engine("glmnet") %>%
#'   nested()
#'
#' fitted <- fit(model, z ~ x + y + a + b, nested_data)
#'
#' multi_predict(fitted, example_nested_data,
#'   penalty = c(0.1, 0.2, 0.3)
#' )
#'
#' @importFrom parsnip multi_predict
#'
#' @export
multi_predict.nested_model_fit <- function(object, new_data, ...) {
  fit <- object$fit

  new_data <- check_df(new_data, "new_data")

  outer_names <- colnames(fit)[colnames(fit) != ".model_fit"]
  inner_names <- object$inner_names

  if (all(!outer_names %in% colnames(new_data))) {
    cli::cli_abort(c(
      "None of the columns used to nest the training set exist in
        {.arg new_data}."
    ))
  } else if (any(!outer_names %in% colnames(new_data))) {
    cli::cli_warn(c(
      "Some of the columns used to nest the training set don't exist in
        {.arg new_data}."
    ))
    outer_names <- outer_names[outer_names %in% colnames(new_data)]
    fit <- fit[, c(outer_names, ".model_fit")] %>%
      tidyr::chop(".model_fit") %>%
      dplyr::mutate(.model_fit = purrr::map(.data$.model_fit, 1))
  }

  data_nest <- nest_data(new_data, inner_names, outer_names)
  nested_data <- data_nest$nested_data
  nested_column <- data_nest$column
  order <- data_nest$order

  model_map <- dplyr::left_join(nested_data, fit, by = outer_names)

  pred <- purrr::map2(
    model_map$.model_fit, model_map[[nested_column]],
    multi_predict_nested, ...
  )

  predictions <- fix_predictions(pred, model_map$data)

  dplyr::bind_rows(predictions)[order, ]
}

multi_predict_nested <- function(model, data, ...) {
  if (is.null(model)) {
    NULL
  } else {
    purrr::possibly(parsnip::multi_predict, 
                    otherwise = NULL, quiet = F)(model, data, ...)
  }
}
