#' Developer utility functions.
#'
#' Not intended for use by the general public
#'
#' @noRd
get_name <- function(name, colnames) {
  vctrs::vec_as_names(c(name, colnames))[1]
}

#' @noRd
preserve_attributes <- function(new_object, old_object) {
  classes <- class(old_object)
  attributes <- attributes(old_object)[
    !names(attributes(old_object)) %in% c("row.names", "names", "class")
  ]
  attributes(new_object) <- c(attributes(new_object), attributes)
  class(new_object) <- unique(c(class(new_object), classes))
  new_object
}

#' @noRd
bind_rows_with_nest_id <- function(list) {
  nest_ids <- glue::glue("nested_data_{1:length(list)}")
  tibble::tibble(nest_id = nest_ids, data = list) %>%
    tidyr::unnest(data) %>%
    preserve_attributes(list[[1]])
}

#' @noRd
get_nested_step_index <- function(recipe) {
  recipe$steps %>%
    purrr::map(class) %>%
    purrr::map_lgl(., ~ {
      "step_nest" %in% .
    }) %>%
    which()
}


#' @noRd
apply_nested_recipe <- function(data, recipe) {
  if (is.null(recipe)) {
    if (is.null(data$data) | purrr::none(data$data, is.data.frame)) {
      cli::cli_abort(c(
        "{.arg new_data} must be nested.", 
        "*" = "The recipe does not contain a nesting step.",
        "i" = "Try using {.fun step_nest}."
      ))
    }
    data
  } else {
    recipe %>%
      recipes::prep(data) %>%
      recipes::bake(data)
  }
}

get_nested_recipe <- function(recipe, index = NULL) {
  if(is.null(index)) {
    if(recipes::detect_step(recipe, "nest")) {
      index <- get_nested_step_index(recipe)
    } else {
      name <- rlang::expr_name(substitute(recipe))
      cli::cli_abort(c(
        "{.arg {name}} does not have a nesting step",
        "i" = "Try using {.fun step_nest}"
      ))
    }
  }
  nested_step <- recipe$steps[[index]]
  
  # creates a recipe that just applies the nest step
  nested_recipe <- recipe
  nested_recipe$steps <- NULL
  recipes::add_step(nested_recipe, nested_step)
}

#' @noRd
remove_class <- function(object, class) {
  class(object) <-
    stringr::str_subset(class(object), glue::glue("^{class}$"), negate = T)
  object
}

# copied from the rlang package
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
