#' @importFrom generics fit_xy
#' 
#' @export
fit_xy.nested_model <- function(object, x, y, case_weights = NULL, 
                                control = control_parsnip(), ...) {
  model <- object$eng_args$model_spec[[1]]
  
  if(!is.null(object$args)) {
    model <- pass_down_args(model, object)
  }
  
  if(!"nest_id" %in% colnames(x)) {
    cli::cli_abort(c(
      "{.arg data} does not contain a \"nest_id\" column.",
      "i" = "Try using {.fun step_nest}."
    ))
  }
  
  y$nest_id <- x$nest_id
  
  nested_x <- x %>%
    dplyr::group_by(.data$nest_id) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  
  nested_y <- y %>%
    dplyr::group_by(.data$nest_id) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  
  fits <- purrr::map2(nested_x$data, nested_y$data, fit_xy, 
                      object = model, case_weights = case_weights,
                      control = control, ...)
  
  nested_x %>%
    dplyr::select(-.data$data) %>%
    dplyr::mutate(.model_fit = .env$fits) %>%
    new_nested_model_fit(spec = model)
}

pass_down_args <- function(inner, outer) {
  inner_args <- inner$args
  outer_args <- outer$args
  keep_inner_args <- inner_args[!names(inner_args) %in% names(outer_args)]
  final_args <- c(keep_inner_args, outer_args)
  inner$args <- final_args
  inner
}

