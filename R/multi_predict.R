#' @importFrom parsnip multi_predict
#' 
#' @export
multi_predict.nested_model_fit <- function(object, new_data, ...) {
  fit <- object$fit
  
  order_name <- get_name(".order", colnames(new_data))
  pred_name <- get_name(".pred", colnames(new_data))
  
  if("nest_id" %in% colnames(data)) {
    with_order <- new_data %>%
      dplyr::mutate(!!order_name := 1:nrow(new_data)) %>%
      tidyr::nest(data = -.data$nest_id)
    
    nested_data <- new_data %>%
      tidyr::nest(data = -.data$nest_id)
    
    nested_data$.model_fit <- NULL
    
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
      with_order <- new_data %>%
        dplyr::mutate(!!order_name := 1:nrow(new_data))
      
      nested_data <- new_data 
    } else {
      with_order <- new_data %>%
        dplyr::mutate(!!order_name := 1:nrow(.env$new_data)) %>%
        tidyr::nest(data = -c(!!!rlang::syms(names)))
      
      nested_data <- new_data %>%
        tidyr::nest(data = -c(!!!rlang::syms(names)))
    }
    
    model_map <- dplyr::left_join(nested_data, fit, by = names)
  }
  
  predictions <- purrr::map2(model_map$.model_fit, model_map$data, 
                             multi_predict_nested, ...)
  
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
  
  dplyr::mutate(with_order, !!pred_name := predictions) %>%
    tidyr::unnest(c(data, .data[[pred_name]])) %>%
    dplyr::arrange(.data[[order_name]]) %>%
    dplyr::select(tidyselect::all_of(format_names))
}


multi_predict_nested <- function(model, data, ...) {
  if(!is.list(model) && is.na(model)) {
    NULL
  } else if(rlang::is_bare_list(model)) {
    predictions <- purrr::map(model, multi_predict_nested, data = data, ...)
    combine_predictions(purrr::compact(predictions))
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

fix_predictions <- function(data, names) {
  purrr::map(names, ~ {rep(NA, nrow(data))}) %>%
    purrr::set_names(names) %>%
    tibble::as_tibble()
}
