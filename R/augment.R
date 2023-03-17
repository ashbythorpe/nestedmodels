#' Augment data with predictions
#'
#' [generics::augment()] method for nested models. `augment.nested_model_fit()`
#' will add column(s) for predictions to the given data.
#'
#' @param x A `nested_model_fit` object produced by
#'   [fit.nested_model()].
#' @param new_data A data frame - can be nested or non-nested.
#' @param ... Passed onto [parsnip::augment.model_fit()].
#'
#' @returns A data frame with one or more added columns for predictions.
#'
#' @seealso [parsnip::augment.model_fit()]
#'
#' @examples
#' 
#' library(dplyr)
#' library(tidyr)
#' library(parsnip)
#' 
#' data <- filter(example_nested_data, id %in% 1:5)
#'
#' nested_data <- nest(data, data = -c(id, id2))
#'
#' model <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   nested()
#'
#' fitted <- fit(model, z ~ x + y + a + b, nested_data)
#'
#' augment(fitted, example_nested_data)
#'
#' @importFrom generics augment
#'
#' @export
augment.nested_model_fit <- function(x, new_data, ...) {
  fit <- x$fit

  new_data <- check_df(new_data, "new_data")

  outer_names <- colnames(fit)[colnames(fit) != ".model_fit"]
  inner_names <- x$inner_names

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

  pred <- purrr::map2(model_map$.model_fit, model_map[[nested_column]],
    augment_nested, ...,
    .inner_names = inner_names
  )

  predictions <- fix_augmented_predictions(pred, model_map[[nested_column]])

  dplyr::bind_rows(predictions)[order, ]
}

?parsnip::fit.model_spec

#' @noRd
augment_nested <- function(model, data, ..., .inner_names) {
  if (is.null(model)) {
    NULL
  } else {
    purrr::possibly(augment, otherwise = NULL, quiet = F)(model, data, ...)
  }
}

#' @noRd
fix_augmented_predictions <- function(predictions, data) {
  invalid_predictions <- purrr::map_lgl(predictions, is.null)
  predictions_format <- predictions[[which(!invalid_predictions)[1]]]
  format_names <- colnames(predictions_format)

  if (all(invalid_predictions)) {
    cli::cli_abort(c(
      "All of the predictions failed."
    ))
  } else if (any(invalid_predictions)) {
    cli::cli_warn(c(
      "Some predictions failed."
    ))
    predictions[invalid_predictions] <-
      purrr::map(data[invalid_predictions],
        fix_augmented_df_predictions,
        names = format_names
      )
  }
  predictions
}

#' @noRd
fix_augmented_df_predictions <- function(data, names) {
  pred_names <- names[!names %in% colnames(data)]
  pred <- purrr::map(pred_names, ~ {
    rep(NA, nrow(data))
  }) %>%
    rlang::set_names(pred_names)
  dplyr::bind_cols(data, pred)
}
