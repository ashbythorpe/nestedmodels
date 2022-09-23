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

stop_bad_length <- function(name, expected, actual, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg {name}} must have length: {.val {expected}}.",
    "i" = "Actual class: {.val {actual}}."
  ), call = call, class = "bad_length")
}

stop_missing <- function(name, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg {name}} must be non-missing."
  ), call = call, class = "missing")
}

stop_length_zero <- function(name, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg {name}} has length 0."
  ), call = call, class = "length_zero")
}

test_logical_arg <- function(x, call = rlang::caller_env()) {
  name <- rlang::expr_name(substitute(x))
  if(!is.logical(x)) {
    stop_bad_type(name, "a logical vector", x, call = call)
  } else if(is.na(x)) {
    stop_missing(name, call = call)
  } else if(length(x) != 1) {
    stop_bad_length(name, 1, length(x), call = call)
  }
}

warn_ambiguous_column <- function(name) {
  cli::cli_warn(c(
    "Ambiguous column to unnest in {.arg new_data}.",
    "Using column {.val {name}}."
  ))
}

stop_not_nested <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg new_data} is not nested.",
    "i" = "Try manually nesting the data with {.fun tidyr::nest}."
  ), call = call, class = "not_nested")
}
