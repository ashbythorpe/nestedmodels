#' Developer utility functions.
#'
#' Not intended for use by the general public
#'
#' @noRd
get_control <- function(object, default) {
  if (inherits(object, "control_nested")) {
    object$control %||% default
  } else {
    object
  }
}

#' @noRd
get_name <- function(name, colnames) {
  vctrs::vec_as_names(c(name, colnames))[1]
}

#' @noRd
exec_with_control <- function(.f, ..., control) {
  if (is.null(control)) {
    rlang::exec(.f, ...)
  } else {
    rlang::exec(.f, ..., control = control)
  }
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
remove_nested_step <- function(workflow, index = NULL) {
  if (is.null(index)) {
    index <- workflow$pre$actions$recipe$recipe %>%
      get_nested_step_index()
  }
  workflow$pre$actions$recipe$recipe$steps[[index]] <- NULL
  # removes the non-nested columns from the recipe
  workflow$pre$actions$recipe$recipe$var_info <-
    workflow$pre$actions$recipe$recipe$var_info %>%
    dplyr::filter(role != "nested_id")
  workflow$pre$actions$recipe$recipe$term_info <-
    workflow$pre$actions$recipe$recipe$term_info %>%
    dplyr::filter(role != "nested_id")
  workflow
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
apply_nested_workflow <- function(object, data = NULL) {
  recipe <- object$pre$actions$recipe$recipe

  if (recipes::detect_step(recipe, "nest")) {

    # gets the index of the nest step in the recipe
    nested_step_index <- get_nested_step_index(recipe)
    
    nested_recipe <- get_nested_recipe(recipe, nested_step_index)
    
    if (!is.null(data)) {
      # applies the recipe to the data to nest it
      nested_data <-
        nested_recipe %>%
        recipes::prep(data) %>%
        recipes::bake(data)
    } else {
      nested_data <- NULL
    }

    # creates a workflow without the nest step
    workflow_without_nested_recipe <- remove_nested_step(object, nested_step_index) %>%
      remove_class("nested_workflow")
  } else {
    # if a nest step doesn't exist, the data is assumed to be already nested

    if (is.null(data$data) | purrr::none(data$data, is.data.frame)) {
      cli::cli_abort(c(
        "{.arg new_data} must be nested.", 
        "*" = "The recipe does not contain a nesting step.",
        "i" = "Try using {.fun step_nest}."
      ))
    }
    nested_recipe <- NULL
    nested_data <- data
    workflow_without_nested_recipe <- object %>%
      remove_class("nested_workflow")
  }

  list(wf = workflow_without_nested_recipe, recipe = nested_recipe, data = nested_data)
}

#' @noRd
combine_model_preprocessor <- function(object, preprocessor) {
  if (inherits(preprocessor, "nested_recipes")) {
    purrr::map(preprocessor, {
      workflows::workflow() %>%
        workflows::add_recipe(.) %>%
        workflows::add_model(object)
    }) %>%
      nested_workflows()
  } else if (inherits(preprocessor, "nested_formulas")) {
    purrr::map(preprocessor, {
      workflows::workflow() %>%
        workflows::add_formula(.) %>%
        workflows::add_model(object)
    }) %>%
      nested_workflows()
  } else if (inherits(preprocessor, "recipe")) {
    workflows::workflow() %>%
      workflows::add_recipe(preprocessor) %>%
      workflows::add_model(object)
  } else if (inherits(preprocessor, "formula")) {
    workflows::workflow() %>%
      workflows::add_formula(preprocessor) %>%
      workflows::add_model(object)
  }
}

#' @noRd
combine_models_preprocessor <- function(object, preprocessor) {
  if (inherits(preprocessor, "nested_recipes")) {
    purrr::map2(object, preprocessor, {
      workflows::workflow() %>%
        workflows::add_recipe(.x) %>%
        workflows::add_model(.y)
    })
  } else if (inherits(preprocessor, "nested_formulas")) {
    purrr::map2(object, preprocessor, {
      workflows::workflow() %>%
        workflows::add_formula(.x) %>%
        workflows::add_model(.y)
    })
  } else if (inherits(preprocessor, "recipe")) {
    purrr::map(object, {
      workflows::workflow() %>%
        workflows::add_recipe(preprocessor) %>%
        workflows::add_model(.)
    })
  } else if (inherits(preprocessor, "formula")) {{
    purrr::map(object, {
      workflows::workflow() %>%
        workflows::add_formula(preprocessor) %>%
        workflows::add_model(.)
    })
  } %>% nested_workflows()  }
}

#' @noRd
remove_class <- function(object, class) {
  class(object) <-
    stringr::str_subset(class(object), glue::glue("^{class}$"), negate = T)
  object
}

# copied from the rlang source code
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
