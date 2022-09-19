#' nestedmodels constructors
#'
#' Constructors for nestedmodels objects. Not intended for use by the general public.
#'
#' @param fit A tibble of model fits.
#' @param spec The original model specification.
#' @param ... Passed on to [structure()]
#'
#' @noRd
new_nested_model_fit <- function(fit, spec, ...) {
  list(spec = spec, fit = fit) %>%
    structure(class = "nested_model_fit", ...)
}

# new_control_nested <- function(x, ...) {
#   structure(x, class = "new_control_nested")
# }
