#' @export
step_group_by <- function(recipe, ..., trained = FALSE, skip = FALSE, 
                          names = NULL, id = recipes::rand_id("group_by")) {
  recipes::recipes_pkg_check(required_pkgs.step_group_by())
  
  recipes::add_step(
    recipe,
    step_group_by_new(
      terms = rlang::enquos(...),
      trained = trained,
      names = names,
      skip = skip,
      id = id
    )
  )
}

step_group_by_new <- function(terms, trained, names, skip, id) {
  recipes::step(
    subclass = "group_by",
    terms = terms,
    trained = trained,
    names = names,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_group_by <- function(x, training, info = NULL, ...) {
  names <- recipes::recipes_eval_select(x$terms, training, info)
  
  step_group_by_new(
    terms = x$terms,
    trained = TRUE,
    names = names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_group_by <- function(object, new_data, ...) {
  cli::cli_warn(c(
    "(.fun nestedmodels::step_group_by} called with no corresponding {.fun nestedmodels::step_ungroup()} step"
  ))
  
  new_data
}

#' @export
print.step_group_by <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Group by "
  recipes::print_step(x$names, x$terms, x$trained, title, width)
  invisible(x)
}

#' @export
required_pkgs.step_group_by <- function(x, ...) {
  c("nestedmodels", "tidyr", "dplyr")
}
