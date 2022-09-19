#' @importFrom generics augment
#' 
#' @export
augment.nested_model <- function(x, new_data, ...) {
  fit <- object$fit
  
  order_name <- get_name(".order", colnames(new_data))
  
  if("nest_id" %in% colnames(data)) {
    order <- new_data %>%
      dplyr::mutate(!!order_name := 1:nrow(new_data)) %>%
      dplyr::arrange(.data$nest_id) %>%
      dplyr::pull(.data[[order_name]])
    
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
      order <- new_data %>%
        dplyr::mutate(!!order_name := 1:nrow(.env$new_data)) %>%
        dplyr::arrange(.data$data) %>%
        dplyr::pull(.data[[order_name]])
      
      nested_data <- tidyr::nest(new_data, data = -.data$data)
    } else {
      order <- new_data %>%
        dplyr::mutate(!!order_name := 1:nrow(.env$new_data)) %>%
        dplyr::arrange(!!!rlang::syms(names)) %>%
        dplyr::pull(.data[[order_name]])
      
      nested_data <- new_data %>%
        tidyr::nest(data = -c(!!!rlang::syms(names)))
    }
    
    model_map <- dplyr::left_join(nested_data, fit, by = names)
  }
  
  predictions <- purrr::map2(model_map$.model_fit, model_map$data, 
                             augment_nested, ...)
  
  invalid_predictions <- purrr::map_lgl(predictions, is.null)
  predictions_format <- predictions[[which(!invalid_predictions)[1]]]
  format_names <- colnames(predictions_format)
  
  if(all(invalid_predictions)) {
    cli::cli_warn(c(
      "All of the predictions failed."
    ))
    return(NULL)
  } else if(any(invalid_predictions)) {
    cli::cli_warn(c(
      "Some predictions failed."
    ))
    predictions[invalid_predictions] <- 
      purrr::map(model_map$data[invalid_predictions],
                 fix_predictions, names = format_names)
  }
  
  final_pred <- dplyr::bind_rows(predictions) %>%
    dplyr::arrange(.env$order)
  
  dplyr::bind_cols(new_data, final_pred)
}
