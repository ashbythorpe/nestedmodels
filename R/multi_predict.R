#' @importFrom parsnip multi_predict
#' 
#' @export
multi_predict.nested_model_fit <- function(object, new_data, ...) {
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
    
  model_map <- dplyr::left_join(nested_data, fit, by = names)
  
  pred <- purrr::map2(model_map$.model_fit, model_map$data, 
                             multi_predict_nested, ...)
  
  predictions <- fix_predictions(pred)
  
  dplyr::bind_rows(predictions)[order,]
}

multi_predict_nested <- function(model, data, ...) {
  if(!is.list(model) && is.na(model)) {
    NULL
  } else if(rlang::is_bare_list(model)) {
    predictions <- purrr::map(model, multi_predict_nested, data = data, ...)
    combine_multi_predictions(purrr::compact(predictions))
  } else {
    multi_predict(model, data, ...)
  }
}

combine_multi_predictions <- function(list) {
  if(length(list) == 0) {
    NULL
  } else if(length(list) == 1) {
    list[[1]]
  } else {
    names <- colnames(list[[1]])
    purrr::pmap(list, combine_multi_prediction_tibbles)
    
    combine_multi_predictions(list)
    purrr::pmap(list, combine_predictions_row) %>%
      rlang::set_names(names) %>%
      tibble::as_tibble()
  }
}

combine_multi_prediction_tibbles <- function(...) {
  l <- list(...)
  pred_colnames <- stringr::str_subset(colnames(..1), "\\.pred")
  other_colnames <- setdiff(colnames(..1), pred_colnames)
  purrr::reduce(l, dplyr::full_join, by = colnames(..1)) %>%
    dplyr::summarise(
      dplyr::across(tidyselect::all_of(other_colnames), ~ .[1]), 
      dplyr::across(tidyselect::all_of(pred_colnames), mean)
    )
}

safe_multi_predict <- function() {}
