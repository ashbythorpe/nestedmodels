#' Create a Nested Model
#'
#' `nested()` turns a model or workflow into a nested model/workflow.
#' `is_nested()` checks if a model or workflow is nested.
#'
#' @param x A model specification or workflow.
#' @param allow_par A logical to allow parallel processing over nests during
#'   the fitting process (if a parallel backend is registered).
#' @param pkgs An optional character string of R package names that should be 
#'   loaded (by namespace) during parallel processing.
#' @param ... Not currently used.
#'
#' @return A nested model object, or a workflow containing a nested model.
#' For `is_nested()`, a logical vector of length 1.
#'
#' @examplesIf rlang::is_installed("workflows")
#' 
#' library(parsnip)
#' library(workflows)
#' 
#' model <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   nested()
#'
#' model
#'
#' is_nested(model)
#'
#' wf <- workflow() %>%
#'   add_model(model)
#'
#' is_nested(wf)
#'
#' @export
nested <- function(x, ...) UseMethod("nested")

#' @rdname nested
#' @export
is_nested <- function(x, ...) UseMethod("is_nested")

#' @rdname nested
#' @export
nested.default <- function(x, ...) {
  stop_bad_class("x", c("model_spec", "workflow"), class(x))
}

#' @rdname nested
#' @export
nested.model_spec <- function(x, allow_par = FALSE, pkgs = NULL, ...) {
  mode <- x$mode
  nested_model(mode, x, allow_par = allow_par, pkgs = NULL)
}

#' @rdname nested
#' @export
nested.nested_model <- function(x, allow_par = FALSE, pkgs = NULL, ...) x

#' @rdname nested
#' @export
nested.workflow <- function(x, allow_par = FALSE, pkgs = NULL, ...) {
  x$fit$actions$model$spec <-
    nested(x$fit$actions$model$spec, allow_par = allow_par, pkgs = NULL, ...)
  x
}

#' @rdname nested
#' @export
is_nested.default <- function(x, ...) FALSE

#' @rdname nested
#' @export
is_nested.model_spec <- function(x, ...) {
  inherits(x, "nested_model")
}

#' @rdname nested
#' @export
is_nested.workflow <- function(x, ...) {
  is_nested(x$fit$actions$model$spec)
}




#' Get the inner model of a nested model object
#'
#' Extract the inner model of a `nested_model` object, or a workflow
#' containing a nested model.
#'
#' @param x A model spec or workflow.
#' @param ... Not used.
#'
#' @returns A `model_spec` object
#'
#' @examples
#' 
#' library(parsnip)
#' 
#' model <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   nested()
#'
#' extract_inner_model(model)
#'
#' @export
extract_inner_model <- function(x, ...) UseMethod("extract_inner_model")

#' @rdname extract_inner_model
#' @export
extract_inner_model.default <- function(x, ...) {
  stop_bad_class("x", c("model_spec", "workflow"), class(x))
}

#' @rdname extract_inner_model
#' @export
extract_inner_model.nested_model <- function(x, ...) {
  x$eng_args$model_spec[[1]]
}

#' @rdname extract_inner_model
#' @export
extract_inner_model.workflow <- function(x, ...) {
  extract_inner_model(x$fit$actions$model$spec)
}

#' @rdname extract_inner_model
#' @export
extract_inner_model.model_spec <- function(x, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a nested model.",
    "i" = "Try using {.fun nested}."
  ))
}
