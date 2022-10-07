#' Nested Model Predictions
#'
#' Apply a fitted nested model to generate different types of predictions.
#' [stats::predict()] / [parsnip::predict_raw()] methods for nested model fits.
#'
#' @param object A `nested_model_fit` object produced by
#'   [fit.nested_model()].
#' @param new_data A data frame to make predictions on. Can be nested or
#'   non-nested.
#' @param type A singular character vector or `NULL`. Passed on to
#'   [parsnip::predict.model_fit()].
#' @param opts A list of optional arguments. Passed on to
#'   [parsnip::predict.model_fit()].
#' @param ... Arguments for the underlying model's predict function. Passed on
#'   to [parsnip::predict.model_fit()].
#'
#' @returns A data frame of model predictions. For `predict_raw()`, a
#'   matrix, data frame, vector or list.
#'
#' @seealso [parsnip::predict.model_fit()]
#'
#' @examples
#' data <- dplyr::filter(example_nested_data, id %in% 5:15)
#'
#' nested_data <- tidyr::nest(data, data = -id)
#'
#' model <- parsnip::linear_reg() %>%
#'   parsnip::set_engine("lm") %>%
#'   nested()
#'
#' fitted <- fit(model, z ~ x + y + a + b, nested_data)
#'
#' predict(fitted, example_nested_data)
#'
#' parsnip::predict_raw(fitted, example_nested_data)
#'
#' @importFrom stats predict
#'
#' @export
predict.nested_model_fit <- function(object, new_data, type = NULL,
                                     opts = list(), ...) {
  fit <- object$fit

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
      tidyr::chop(.data$.model_fit) %>%
      dplyr::mutate(.model_fit = purrr::map(.data$.model_fit, 1))
  }

  data_nest <- nest_data(new_data, inner_names, outer_names)
  nested_data <- data_nest$nested_data
  nested_column <- data_nest$column
  order <- data_nest$order

  model_map <- dplyr::left_join(nested_data, fit, by = outer_names)

  pred <- purrr::map2(model_map$.model_fit, model_map[[nested_column]],
    predict_nested,
    type = type, opts = opts, ...
  )

  predictions <- fix_predictions(pred, model_map$data)

  if (is.data.frame(predictions[[1]])) {
    dplyr::bind_rows(predictions)[order, ]
  } else if (is.vector(predictions[[1]])) {
    vctrs::vec_c(!!!predictions)[order]
    # Switch to purrr::list_c() when that releases
  } else if (is.matrix(predictions[[1]])) {
    vctrs::vec_rbind(!!!predictions)
  } else {
    cli::cli_warn(c(
      "Prediction format not recognised. Returning list of results."
    ))
    predictions
  }
}

#' @importFrom parsnip predict_raw
#'
#' @rdname predict.nested_model_fit
#' @export
predict_raw.nested_model_fit <- function(object, new_data, opts = list(), ...) {
  predict(object, new_data = new_data, type = "raw", opts = opts, ...)
}

#' @noRd
predict_nested <- function(model, data, ...) {
  if (is.null(model)) {
    NULL
  } else {
    purrr::possibly(predict, otherwise = NULL, quiet = F)(model, data, ...)
  }
}

#' @noRd
fix_predictions <- function(pred, data) {
  invalid_predictions <- purrr::map_lgl(pred, is.null)
  predictions_format <- pred[[which(!invalid_predictions)[1]]]

  if (all(invalid_predictions)) {
    cli::cli_abort(c(
      "All of the predictions failed."
    ))
  } else if (any(invalid_predictions)) {
    cli::cli_warn(c(
      "Some predictions failed."
    ))
    if (is.data.frame(predictions_format)) {
      format_names <- colnames(predictions_format)
      pred[invalid_predictions] <-
        purrr::map(data[invalid_predictions],
          fix_df_predictions,
          names = format_names
        )
    } else if (is.vector(predictions_format)) {
      pred[invalid_predictions] <-
        purrr::map(
          data[invalid_predictions],
          fix_vector_predictions
        )
    } else if (is.matrix(predictions_format)) {
      format_names <- colnames(predictions_format)
      pred[invalid_predictions] <-
        purrr::map(data[invalid_predictions],
          fix_matrix_predictions,
          names = format_names,
          ncol = ncol(predictions_format)
        )
    }
  }
  pred
}

#' @noRd
fix_df_predictions <- function(data, names) {
  purrr::map(names, ~ {
    rep(NA, nrow(data))
  }) %>%
    purrr::set_names(names) %>%
    tibble::as_tibble()
}

#' @noRd
fix_vector_predictions <- function(data) {
  rep(NA, nrow(data))
}

#' @noRd
fix_matrix_predictions <- function(data, names, ncol) {
  if (is.null(names)) {
    dimnames <- NULL
  } else {
    dimnames <- list(NULL, names)
  }
  matrix(
    nrow = nrow(data), ncol = ncol,
    dimnames = dimnames
  )
}
