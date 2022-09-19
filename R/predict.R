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
  
  if("nest_id" %in% colnames(data)) {
    format_data <- new_data %>%
      dplyr::mutate(!!order_name := 1:nrow(new_data)) %>%
      dplyr::arrange(.data$nest_id)
    
    nested_data <-
      tidyr::nest(new_data, data = -.data$nest_id)
    
    model_map <- dplyr::left_join(nested_data, fit, by = "nest_id")
  } else {
    names <- colnames(fit)[-ncol(fit)]
    
    if(all(!names %in% colnames(new_data))) {
      cli::cli_abort(c(
        "None of the columns used to nest the training set exist in 
        {.arg new_data}."
      ))
    } else if(any(!names %in% colnames(new_data))) {
      cli::cli_warn(c(
        "Some of the columns used to nest the training set don't exist in
        {.arg new_data}."
      ))
      names <- names[names %in% colnames(new_data)]
      fit <- fit[,c(names, ".model_fit")] %>%
        tidyr::chop(.model_fit)
    }
    
    if("data" %in% colnames(new_data) && 
       purrr::every(new_data$data, is.data.frame)) {
      format_data <- new_data %>%
        dplyr::mutate(!!order_name := 1:nrow(.env$new_data)) %>%
        dplyr::arrange(.data$data)
      
      nested_data <- tidyr::nest(new_data, data = -.data$data)
    } else {
      format_data <- new_data %>%
        dplyr::mutate(!!order_name := 1:nrow(.env$new_data)) %>%
        dplyr::arrange(!!!rlang::syms(names))
      
      nested_data <- new_data %>%
        tidyr::nest(data = -c(!!!rlang::syms(names)))
    }
    
    model_map <- dplyr::left_join(nested_data, fit, by = names)
  }
  
  predictions <- purrr::map2(model_map$.model_fit, model_map$data, 
                             predict_nested,
                             type = type, opts = opts, ...)
  
  invalid_predictions <- purrr::map_lgl(predictions, is.null)
  predictions_format <- predictions[[which(!invalid_predictions)[1]]]
  format_names <- colnames(predictions_format)
  
  if(all(invalid_predictions)) {
    cli::cli_abort(c(
      "All of the predictions failed."
    ))
  } else if(any(invalid_predictions)) {
    cli::cli_warn(c(
      "Some predictions failed."
    ))
    predictions[invalid_predictions] <- 
      purrr::map(model_map$data[invalid_predictions],
                 fix_predictions, names = format_names)
  }
  
  dplyr::bind_cols(format_data, dplyr::bind_rows(predictions)) %>%
    dplyr::arrange(.data[[order_name]]) %>%
    dplyr::select(tidyselect::all_of(format_names))
}


predict_nested <- function(model, data, ...) {
  if(!is.list(model) && is.na(model)) {
    NULL
  } else if(rlang::is_bare_list(model)) {
    predictions <- purrr::map(model, predict_nested, data = data, ...)
    combine_predictions(purrr::compact(predictions))
  } else {
    safe_predict(model, data, ...)
  }
}

combine_predictions <- function(list) {
  if(length(list) == 0) {
    NULL
  } else {
    names <- colnames(list[[1]])
    purrr::pmap(list, combine_predictions_row) %>%
      rlang::set_names(names) %>%
      tibble::as_tibble()
  }
}

combine_predictions_row <- function(...) {
  data.frame(...) %>%
    rowMeans()
}

safe_predict <- function() {}

fix_predictions <- function(data, names) {
  purrr::map(names, ~ {rep(NA, nrow(data))}) %>%
    purrr::set_names(names) %>%
    tibble::as_tibble()
}

# predict.nested_workflow_fit <- function(object, new_data,
#                                             type = NULL,
#                                             opts = list(), ...) {
#   nested_recipe <- attr(object, "nested_recipe")
# 
#   if (!inherits(nested_recipe, "recipe")) {
#     rlang::abort("The 'nested_recipe' attribute of `object` must be a recipe")
#   }
# 
#   nested_data <- apply_nested_recipe(new_data, nested_recipe)
# 
#   # the predictions are made on each nested data set
#   purrr::map2(
#     object, nested_data$data,
#     predict, type, opts, ...
#   ) %>%
#     dplyr::bind_rows()
# }
# 
# predict.nested_model_fit <- function(object, new_data,
#                                          type = NULL, opts = list(),
#                                          ...) {
#   # the predictions are made on each data set
#   purrr::map2(
#     object, new_data$data,
#     predict, type, opts, ...
#   ) %>%
#     dplyr::bind_rows()
# }

nest_data_with_model <- function(data, model) {
  names <- colnames(model)[-ncol(model)]
  
  if(all(!names %in% data)) {
    cli::cli_abort("")
  }
}
