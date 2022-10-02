#' Nest transformation
#'
#' `step_nest` creates a *specification* of a recipe step that will
#' convert specified data into a single model term, specifying the 'nest'
#' that each row of the dataset corresponds to.
#'
#' @param recipe A recipe object. The step will be added to the
#'   sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'   For `step_nest`, this indicates the variables which will *not* be
#'   nested. See [recipes::selections()] for more details.
#' @param role Not used by this step since the new variables are assigned a
#'   custom role.
#' @param trained A logical to indicate if the quantities for
#'   preprocessing have been estimated.
#' @param skip A logical. Should the step be skipped when the
#'   recipe is baked by [bake()]? While all operations are baked
#'   when [prep()] is run, some operations may not be able to be
#'   conducted on new data (e.g. processing the outcome variable(s)).
#'   Care should be taken when using `skip = TRUE` as it may affect
#'   the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @details
#' `step_nest()` will create a single nominal variable (named 'nest_id')
#' from a set of variables (of any type). Every unique combination
#' of the specified columns will recieve a single nest id.
#'
#' This recipe step is designed for use with nested models, since a model
#' will be fitted on the data corresponding to each nest id. Using a recipe
#' is often easier and more reliable than nesting the data manually.
#'
#' The nest id corresponding to each unique combination of column values is
#' decided when the recipe is prepped (if this recipe is contained in a
#' workflow, this happens when the workflow is fitted). This means that
#' when using a prepped recipe on new data (using [recipes::prep()] or
#' [workflows::predict.workflow()]), all unique combinations of nesting
#' columns must also exist in the training data. You will be warned if
#' this is not the case. If you are using the 'rsample' package to create
#' splits and this presents an issue, you may want to consider using
#' [nested_resamples()].
#'
#' `step_nest()` is designed so that nesting the transformed data by its
#' 'nest_id' column is equivalent to the following action on the
#' non-transformed data:
#' ```
#' data %>%
#'   dplyr::group_by(...) %>% # ... represents your specified terms
#'   tidyr::nest()
#' ```
#'
#' @returns An updated version of recipe with the new step added to the
#' sequence of any existing operations.
#'
#' @examples
#' data("example_nested_data")
#'
#' recipe <- recipes::recipe(example_nested_data, z ~ x + id) %>%
#'   step_nest(-id) # equivalent to tidyr::nest(example_nested_data, data = -id)
#'
#' data <- recipe %>%
#'   recipes::prep() %>%
#'   recipes::bake(NULL)
#'
#' recipe2 <- recipes::recipe(example_nested_data, z ~ x + id) %>%
#'   step_nest(x, z)
#' # equivalent to tidyr::nest(example_nested_data, data = c(x,z))
#'
#' data2 <- recipe %>%
#'   recipes::prep() %>%
#'   recipes::bake(NULL)
#'
#' identical(data1, data2) # TRUE
#'
#' @importFrom recipes prep bake
#'
#' @export
step_nest <- function(recipe, ..., role = "predictor", trained = FALSE,
                      names = NULL, lookup_table = NULL,
                      skip = FALSE, id = recipes::rand_id("nest")) {
  recipes::add_step(
    recipe,
    step_nest_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      names = names,
      lookup_table = lookup_table,
      skip = skip,
      id = id
    )
  )
}

step_nest_new <- function(terms, role, trained, names, lookup_table, skip,
                          id) {
  recipes::step(
    subclass = "nest",
    terms = terms,
    role = role,
    trained = trained,
    names = names,
    lookup_table = lookup_table,
    skip = skip,
    id = id,
    names = names
  )
}

#' @rdname step_nest
#' @export
prep.step_nest <- function(x, training, info = NULL) {
  names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(names) > 0) {
    lookup_table <- training %>%
      tidyr::nest(data = -c(!!!rlang::syms(names))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nest_id = glue::glue("Nest {1:dplyr::n()}")) %>%
      dplyr::select(-.data$data)
  } else {
    lookup_table <- NULL
  }

  step_nest_new(
    terms = x$terms,
    trained = TRUE,
    names = names,
    lookup_table = lookup_table,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @rdname step_nest
#' @export
bake.step_nest <- function(object, new_data, ...) {
  names <- object$names
  lookup_table <- object$lookup_table

  if (is.null(lookup_table)) {
    new_data$nest_id <- NA_character_
    return(new_data)
  }

  res <- dplyr::left_join(new_data, lookup_table, by = names) %>%
    dplyr::select(-dplyr::all_of(names)) %>%
    tibble::as_tibble()

  good_models <- purrr::discard(res$nest_id, is.na)
  if (length(good_models) == 0) {
    cli::cli_warn(c(
      "{.arg new_data} contains no nests from the training set."
    ))
  } else if (length(good_models) < nrow(res)) {
    cli::cli_warn(c(
      "Not all of the nests in {.arg new_data} occurred in the training set."
    ))
  }
  res
}
