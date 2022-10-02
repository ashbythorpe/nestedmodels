#' Constructor for 'nested_model' objects
#'
#' Construct a 'nested_model' object.
#'
#' @param mode The model mode (e.g. 'regression')
#' @param model_spec A `model_spec` object.
#'
#' @returns An object with classes `nested_model` and `model_spec`
#'
#' @noRd
nested_model <- function(mode, model_spec) {
  eng_args <- list(model_spec = list(model_spec))

  tune_args <- generics::tune_args(model_spec)$name
  if (length(tune_args) != 0) {
    args <- model_spec$args[tune_args]
  } else {
    args <- NULL
  }

  parsnip::new_model_spec(
    "nested_model",
    args = args,
    eng_args = eng_args,
    mode = mode,
    method = NULL,
    engine = "nestedmodels"
  )
}

#' @importFrom generics tunable
#'
#' @export
tunable.nested_model <- function(x, ...) {
  spec <- x$eng_args$model_spec[[1]]
  res <- tunable(spec, ...)
  res$component <- "nested_model"
  res
}

#' @importFrom stats update
#'
#' @export
update.nested_model <- function(object, ...) {
  spec <- object$eng_args$model_spec[[1]]
  updated_spec <- update(spec, ...)
  nested(updated_spec)
}

#' @importFrom parsnip translate
#'
#' @export
translate.nested_model <- function(x, ...) {
  translate(x$eng_args$model_spec[[1]])
}

#' @importFrom generics required_pkgs
#'
#' @export
required_pkgs.nested_model <- function(x, ...) {
  c("nestedmodels", required_pkgs(x$eng_args$model_spec[[1]], ...))
}

#' @export
print.nested_model <- function(x, ...) {
  cat("Nested Model Specification\n\nInner model:\n")
  print(x$eng_args$model_spec[[1]], ...)
  invisible(x)
}
