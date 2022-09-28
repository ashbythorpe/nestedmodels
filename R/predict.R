#' Nested Model Predictions
#'
#' Apply a fitted nested models to generate different types of predictions.
#' [stats::predict()] methods for nested model fits.
#' 
#' @param object A `nested_model_fit` object produced by
#'  [fit.nested_model_spec()].
#' @param new_data A data frame to make predictions on. Can be nested or
#'  non-nested.
#' @param type A singular character vector or NULL. Passed on to 
#'  [parsnip::predict.model_fit()].
#' @param opts A list of optional arguments. Passed on to 
#'  [parsnip::predict.model_fit()].
#' @param ... Arguments for the underlying model's predict function. Passed on 
#'  to [parsnip::predict.model_fit()].
#'
#' @returns A data frame of model predictions. For `predict_raw()`, a
#' matrix, data frame, vector or list.
#'
#' @seealso [parsnip::predict.model_fit()]
#'
#' @examples
#' data("example_nested_data")
#'
#' model <- parsnip::linear_reg() %>%
#'   parsnip::set_engine("lm") %>%
#'   nested()
#'
#' nested_data <- tidyr::nest(example_nested_data, data = -id)
#'
#' fitted <- fit(model, nested_data)
#'
#' predict(fitted, example_nested_data)
#'
#' predict_raw(fitted, example_nested_data)
#'
#' @importFrom stats predict
#'
#' @export
predict.nested_model_fit <- function(object, new_data, type = NULL, 
                                     opts = list(), ...) {
  fit <- object$fit
  
  order_name <- get_name(".order", colnames(new_data))
  pred_name <- get_name(".pred", colnames(new_data))
  
  outer_names <- colnames(fit)[colnames(fit) != ".model_fit"]
  inner_names <- object$inner_names
  
  if(all(!outer_names %in% colnames(new_data))) {
    cli::cli_abort(c(
      "None of the columns used to nest the training set exist in 
        {.arg new_data}."
    ))
  } else if(any(!outer_names %in% colnames(new_data))) {
    cli::cli_warn(c(
      "Some of the columns used to nest the training set don't exist in
        {.arg new_data}."
    ))
    outer_names <- outer_names[outer_names %in% colnames(new_data)]
    fit <- fit[,c(outer_names, ".model_fit")] %>%
      tidyr::chop(.model_fit) %>%
      dplyr::mutate(.model_fit = .model_fit[[1]])
  }
  
  data_nest <- nest_data(new_data, inner_names, outer_names)
  nested_data <- data_nest$nested_data
  unnested_data <- data_nest$unnested_data
  nested_column <- data_nest$column
  order <- data_nest$order
  
  model_map <- dplyr::left_join(nested_data, fit, by = outer_names)
  
  pred <- purrr::map2(model_map$.model_fit, model_map$data, 
                             predict_nested,
                             type = type, opts = opts, ...)
  
  predictions <- fix_predictions(pred)
  
  if(is.data.frame(predictions[[1]])) {
    dplyr::bind_rows(predictions)[order,]
  } else if(is.vector(predictions[[1]])) {
    vctrs::vec_c(!!!predictions)[order]
    # Switch to purrr::list_c() when that releases
  } else if(is.matrix(predictions[[1]])){
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

predict_nested <- function(model, data, ...) {
  if(is.null(model)) {
    NULL
  } else {
    safe_predict(model, data, ...)
  }
}

fix_predictions <- function(pred) {
  invalid_predictions <- purrr::map_lgl(pred, is.null)
  predictions_format <- pred[[which(!invalid_predictions)[1]]]
  
  if(all(invalid_predictions)) {
    cli::cli_abort(c(
      "All of the predictions failed."
    ))
  } else if(any(invalid_predictions)) {
    cli::cli_warn(c(
      "Some predictions failed."
    ))
    if(is.data.frame(predictions_format)) {
      format_names <- colnames(predictions_format)
      pred[invalid_predictions] <- 
        purrr::map(model_map$data[invalid_predictions],
                   fix_df_predictions, names = format_names)
    } else if(is.vector(predictions_format)) {
      pred[invalid_predictions] <-
        purrr::map(model_map$data[invalid_predictions],
                   fix_vector_predictions)
    } else if(is.matrix(predictions_format)) {
      format_names <- colnames(predictions_format)
      pred[invalid_predictions] <- 
        purrr::map(model_map$data[invalid_predictions],
                   fix_matrix_predictions, names = format_names)
    }
  }
  pred
}

fix_df_predictions <- function(data, names) {
  purrr::map(names, ~ {rep(NA, nrow(data))}) %>%
    purrr::set_names(names) %>%
    tibble::as_tibble()
}

fix_vector_predictions <- function(data) {
  rep(NA, nrow(data))
}

fix_matrix_predictions <- function(data, names) {
  if(is.null(names)) {
    purrr::map(seq_len(ncol(data)), ~ {rep(NA, nrow(data))}) %>%
      as.data.frame() %>%
      as.matrix()
  } else {
    purrr::map(names, ~ {rep(NA, nrow(data))}) %>%
      purrr::set_names(names) %>%
      as.data.frame() %>%
      as.matrix()
  }
}

ncol(matrix(c(1,2,3)))

safe_predict <- function() {} # nocov