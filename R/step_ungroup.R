#' @export
step_ungroup <- function(recipe, ..., trained = FALSE, skip = FALSE, 
                         names = NULL, id = recipes::rand_id("ungroup")) {
  recipes::recipes_pkg_check(required_pkgs.step_ungroup())
  
  res <- recipes::add_step(
    recipe,
    step_ungroup_new(
      terms = rlang::enquos(...),
      trained = trained,
      names = names,
      skip = skip,
      id = id
    )
  )
  
  first_group_by <- purrr::detect_index(
    res$steps, inherits, "step_group_by"
  )
  
  first_grouped <- purrr::detect_index(
    res$steps, inherits, "step_grouped"
  )
  
  if (first_group_by == 0) {
    index <- first_grouped
  } else if (first_grouped == 0) {
    index <- first_group_by
  } else {
    index <- min(first_group_by, first_grouped)
  }
  
  if(index != 0) {
    indexes <- seq(index, length(res$steps))
    inner_steps <- res$steps[indexes]
    res$steps[indexes] <- NULL
    res <- step_grouped(res, inner_steps = inner_steps)
  }
  
  res
}

step_ungroup_new <- function(terms, trained, names, skip, id) {
  recipes::step(
    subclass = "ungroup",
    terms = terms,
    trained = trained,
    names = names,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_ungroup <- function(x, training, info = NULL, ...) {
  if(length(x$terms) == 0) {
    terms <- rlang::quos(dplyr::everything())
  } else {
    terms <- x$terms
  }
  
  names <- recipes::recipes_eval_select(terms, training, info)
  
  step_ungroup_new(
    terms = x$terms,
    trained = TRUE,
    names = names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ungroup <- function(object, new_data, ...) {
  new_data
}

#' @export
print.step_ungroup <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Ungroup "
  recipes::print_step(x$names, x$terms, x$trained, title, width)
  invisible(x)
}

#' @export
required_pkgs.step_ungroup <- function(x, ...) {
  c("nestedmodels", "tidyr", "dplyr")
}


