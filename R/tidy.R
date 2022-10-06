#' Turn a nested model into a tidy tibble
#'
#' @description
#' Use broom functions on fitted nested models.
#'
#' `tidy.nested_model_fit()` summarises components of each model within a
#' nested model fit, indicating which nested data frame each row corresponds
#' to.
#'
#' `glance.nested_model_fit()` summarises a nested model, returning a
#' [tibble::tibble()] with 1 row.
#'
#' `glance_nested()` summarises each model within a nested model fit,
#' returning a [tibble::tibble()] with the same number of rows as the number
#' of inner models.
#'
#' @param x A `nested_model_fit` object produced by [fit.nested_model()].
#' @param ... Additional arguments passed into their respective functions.
#'   (e.g. for `tidy.nested_model_fit()`, [parsnip::tidy.model_fit()]).
#'
#' @details
#' [generics::glance()] states that `glance()` methods should always return 1
#' row outputs for non-empty inputs. The 'nestedmodels' package is no
#' exception: `glance()` methods will combine rows to produce a result with a
#' single row. Specifically:
#' * If a column contains 1 unique value, that value is used.
#' * If a column is numeric, the mean will be calculated.
#' * Otherwise, the results will be combined into a list.
#'
#' @returns A [tibble::tibble()]. With `glance.nested_model_fit()`, the
#' tibble will have 1 row.
#'
#' @seealso [generics::tidy()] [generics::glance()]
#'
#' @examples
#' if (require("broom")) {
#'   data <- dplyr::filter(example_nested_data, id %in% 1:5)
#'
#'   model <- parsnip::linear_reg() %>%
#'     parsnip::set_engine("lm") %>%
#'     nested()
#'
#'   fit <- fit(
#'     model, z ~ x + y + a + b,
#'     dplyr::group_by(data, id)
#'   )
#'
#'   tidy(fit)
#'   glance(fit)
#'   glance_nested(fit)
#' }
#'
#' @importFrom generics tidy
#' @importFrom generics glance
#'
#' @export
tidy.nested_model_fit <- function(x, ...) {
  fit <- x$fit
  tidy_name <- get_name(".tidied", colnames(fit))
  fit[[tidy_name]] <- purrr::map(fit$.model_fit, tidy, ...)
  fit %>%
    dplyr::select(-.data$.model_fit) %>%
    tidyr::unnest(.data[[tidy_name]])
}

#' @rdname tidy.nested_model_fit
#' @export
glance.nested_model_fit <- function(x, ...) {
  purrr::map(x$fit$.model_fit, glance) %>%
    dplyr::bind_rows() %>%
    purrr::map(combine_nested_rows) %>%
    tibble::as_tibble()
}

#' @rdname tidy.nested_model_fit
#' @export
glance_nested <- function(x, ...) {
  if (!inherits(x, "nested_model_fit")) {
    stop_bad_class("x", "nested_model_fit", class(x))
  }

  fit <- x$fit
  tidy_name <- get_name(".tidied", colnames(fit))
  fit[[tidy_name]] <- purrr::map(fit$.model_fit, glance, ...)
  fit %>%
    dplyr::select(-.data$.model_fit) %>%
    tidyr::unnest(.data[[tidy_name]])
}

combine_nested_rows <- function(x) {
  if (length(unique(x)) == 1) {
    x[1]
  } else if (is.numeric(x)) {
    mean(x, na.rm = TRUE)
  } else {
    list(x)
  }
}
