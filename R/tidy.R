#' Tidy, augment and glance methods for nested objects
#'
#' These methods allow the usage of `broom` functions on nested objects.
#'
#' @param x The object to be converted into a tidy [tibble::tibble()].
#' @param ... Additional arguments passed into their respective functions.
#' (e.g. for `tidy.nested_model_fit()`, [parsnip::tidy.model_fit()]).
#'
#' @details
#' Tidying a nested object generally involves applying an already defined 
#' `broom` method to a set of nested data frames or a set of objects produced 
#' from nested data frames, and then combining the result back into a tibble.
#' 
#' @section Glance:
#' [generics::glance()] states that `glance` methods should always return 1
#' row outputs for non-empty inputs. The 'nestedmodels' package is no 
#' exception: `glance` methods will combine rows to produce a result with a
#' single row. Specifically:
#' * If a column contains 1 unique value, that value is used.
#' * If a column is numeric, the mean will be calculated.
#' * Otherwise, the results will be combined into a list.
#' 
#' This may not give you the most useful of results.
#' If you want to generate non-combined glances about every nested data frame,
#' use:
#' `purrr::map(x, glance)`, `lapply(x, glance)`
#' or similar.
#' 
#' @returns A [tibble::tibble()]. With `glance()`, the tibble will have 1 row.
#'
#' @examples
#' data("example_nested_data")
#' resamples <- nested_resamples(id, rsample::vfold_cv, example_nested_data)
#' tidy(resamples)
#'
#' @importFrom generics tidy
#' @importFrom generics glance
#' 
#' @export
tidy.nested_model_fit <- function(x, ...) {
  purrr::map(x$fit$.model_fit, broom::tidy, ...) %>%
    bind_rows_with_nest_id()
}

#' @rdname tidy.nested_model_fit
#' @export
glance.nested_model_fit <- function(x, ...) {
  purrr::map(x$fit$.model_fit, glance) %>%
    purrr::pmap(combine_nested_rows) %>%
    tibble::as_tibble()
}

combine_nested_rows <- function(...) {
  x <- c(...)
  if (is.numeric(x)) {
    mean(x, na.rm = T)
  } else if (length(unique(x)) == 1) {
    x
  } else {
    list2(!!!x)
  }
}
