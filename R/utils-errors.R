stop_bad_class <- function(name, expected, actual, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg {name}} must have class: {.cls {expected}}.",
    "i" = "Actual class: {.cls {actual}}."
  ), call = call, class = "bad_class")
}

stop_bad_type <- function(name, expected, actual, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg {name}} must be {expected}, not {.obj_type_friendly {actual}}."
  ), call = call, class = "bad_type")
}

warn_ambiguous_column <- function(name, colname) {
  cli::cli_warn(c(
    "Ambiguous column to unnest in {.arg {name}}.",
    "Using column {.val {name}}."
  ))
}

stop_not_nested <- function(name, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg {name}} is not nested.",
    "i" = "Try manually nesting the data with {.fun tidyr::nest}."
  ), call = call, class = "not_nested")
}
