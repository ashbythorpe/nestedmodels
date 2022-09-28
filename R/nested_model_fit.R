#' nested_model_fit objects
#' 
#' @param fit A tibble of model fits.
#' @param spec The original model specification.
#' @param inner_names A character vector of names
#' @param ... Passed on to [structure()]
#'
#' @noRd
new_nested_model_fit <- function(fit, spec, inner_names, ...) {
  list(spec = spec, fit = fit, inner_names = inner_names) %>%
    structure(class = c("nested_model_fit", "model_fit"), ...)
}

#' @export
required_pkgs.nested_model_fit <- function(x, ...) {
  c("nestedmodels", required_pkgs(x$spec, ...))
}

#' @export
print.nested_model_fit <- function(x, ...) {
  cat("Nested model fit\n")
  print(x$fit, ...)
  invisible(x)
}
