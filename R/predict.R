#' Nested Model predictions
#'
#' [stats::predict()] methods for nested models and workflows
#'
#' @param object An object with class 'nested_model_fit' or 
#'  'nested_workflow_fit'
#' @param new_data A data frame to make predictions on.
#' @param type A singular character vector or NULL. Passed on to 
#'  [parsnip::predict.model_fit()] or [workflows::predict.workflow()].
#' @param opts A list of optional arguments. Passed on to 
#'  [parsnip::predict.model_fit()] or [workflows::predict.workflow()].
#' @param ... Arguments for the underlying model's predict function. Passed on 
#'  to [parsnip::predict.model_fit()] or [workflows::predict.workflow()].
#'
#' @returns A data frame of model predictions.
#'
#' @seealso [parsnip::predict.model_fit()] [workflows::predict.workflow()]
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
#' fitted <- fit(wf, example_nested_data)
#'
#' predict(fitted, example_nested_data)
#'
#' @importFrom stats predict
#'
#' @export
predict.nested_model_fit <- function(object, new_data, type = NULL, 
                                     opts = list(), ...) {
  fit <- object$fit
  
  order_name <- get_name(".order", colnames(new_data))
  pred_name <- get_name(".pred", colnames(new_data))
  
  outer_names <- colnames(fit)[-ncol(fit)]
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
      tidyr::chop(.model_fit)
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
    vctrs::vec_c(!!!predictions)[order,]
    # Switch to purrr::list_c() when that releases
  } else {
    cli::cli_warn(c(
      "Prediction format not recognised. Returning list of results."
    ))
    predictions
  }
}

predict_nested <- function(model, data, ...) {
  if(!is.list(model) && is.na(model)) {
    NULL
  } else if(rlang::is_bare_list(model)) {
    predictions <- purrr::map(model, predict_nested, data = data, ...) %>%
      purrr::compact()
    if(length(predictions) == 0) {
      return(NULL)
    } else if (is.data.frame(predictions[[1]])) {
      combine_predictions(predictions)
    } else if(is.vector(predictions[[1]])){
      combine_vector_predictions(predictions)
    } else {
      list[[1]]
    }
  } else {
    safe_predict(model, data, ...)
  }
}

combine_predictions <- function(list) {
  if(length(list) == 0) {
    NULL
  } else if(length(list) == 1) {
    list[[1]]
  } else {
    names <- colnames(list[[1]])
    purrr::pmap(list, combine_predictions_row) %>%
      rlang::set_names(names) %>%
      tibble::as_tibble()
  }
}

combine_predictions_row <- function(...) {
  rowMeans(data.frame(...))
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
    }
  }
}

fix_df_predictions <- function(data, names) {
  purrr::map(names, ~ {rep(NA, nrow(data))}) %>%
    purrr::set_names(names) %>%
    tibble::as_tibble()
}

combine_vector_predictions <- function(list) {
  rowMeans(as.data.frame(list))
}

fix_vector_predictions <- function(data) {
  rep(NA, nrow(data))
}


safe_predict <- function() {} # nocov
