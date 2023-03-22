step_grouped <- function(recipe, ..., trained = FALSE, role = NA,
                         inner_steps = list(), step_sequences = NULL, 
                         groups = NULL, skip = FALSE, 
                         id = recipes::rand_id("grouped")) {
  recipes::add_step(
    recipe,
    step_grouped_new(
      trained = trained,
      role = role,
      inner_steps = flatten_steps(inner_steps),
      step_sequences = step_sequences,
      groups = groups,
      skip = skip,
      id = id
    )
  )
}

step_grouped_new <- function(trained, role, inner_steps, step_sequences, 
                             groups, skip, id) {
  recipes::step(
    subclass = "grouped",
    trained = trained,
    role = role,
    inner_steps = inner_steps,
    step_sequences = step_sequences,
    groups = groups,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_grouped <- function(x, training, info = NULL, ...) {
  inner_steps <- x$inner_steps
  
  group_steps <- which(purrr::map_lgl(inner_steps, is_grouping_step))
  
  inner_steps <- purrr::map_at(
    inner_steps, 
    group_steps, 
    recipes::prep,
    training = training,
    info = info,
    ...
  )
  
  rel_steps <- inner_steps[group_steps]
  
  groups <- purrr::accumulate(rel_steps, add_group_variables, .init = character())[-1]
  
  final_groups <- groups[[length(groups)]]
  
  if(length(final_groups) != 0) {
    cli::cli_warn(
      "Not all columns grouped by {.fun nestedmodels::step_group_by} have been ungrouped",
      "x" = "{.var {final_groups}} were not ungrouped and will not be used",
      "i" = "Use {.fun nestedmodels::step_ungroup} with no arguments to ungroup all columns"
    )
    
    groups <- purrr::map(groups, function(x) x[!x %in% final_groups])
  }
  
  groups <- groups[-length(groups)]
  
  indices <- purrr::map2(
    group_steps[-length(group_steps)], 
    group_steps[-1],
    seq
  )
  
  step_sequences <- vctrs::vec_chop(inner_steps, indices = indices)
  
  step_sequences <- purrr::map(step_sequences, purrr::discard, is_grouping_step)
  
  stopifnot(length(step_sequences) == length(groups))
  
  is_empty <- purrr::map_lgl(step_sequences, function(x) length(x) == 0)
  
  groups <- groups[!is_empty]
  
  step_sequences <- step_sequences[!is_empty]
  
  if(length(groups) == 0) {
    return(step_grouped_new(
      trained = TRUE,
      role = NA,
      inner_steps = inner_steps,
      step_sequences = NULL,
      groups = NULL,
      skip = x$skip,
      id = x$id
    ))
  }
  
  runs <- vctrs::vec_identify_runs(groups)
  n_runs <- attr(runs, "n")
  
  groups <- purrr::map(unname(split(groups, runs)), 1)
  
  step_sequences <- purrr::map(unname(split(step_sequences, runs)), purrr::list_c)
  
  step_sequences <- purrr::map(purrr::accumulate2(
    step_sequences, groups, .init = list(training, NULL),
    function(l, s, groups) prep_nested_steps(s, groups, l[[1]], info, ...)
  )[-1], 2)
  
  inner_steps[purrr::map_lgl(inner_steps, purrr::negate(is_grouping_step))] <-
    purrr::map(purrr::flatten(step_sequences), 1)
  
  step_grouped_new(
    trained = TRUE,
    role = NA,
    inner_steps = inner_steps,
    step_sequences = step_sequences,
    groups = groups,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_grouped <- function(object, new_data, ...) {
  if(is.null(object$step_sequences)) {
    return(new_data)
  }
  
  purrr::reduce2(
    object$step_sequences, object$groups, .init = new_data,
    bake_nested_steps, ...
  )
}

#' @export
print.step_grouped <- function(x, width = max(20, options()$width - 30), ...) {
  purrr::walk(x$inner_steps, print_operation, width, ...)
  invisible(x)
}

print_operation <- function(.step, width, ...) {
  print_fun <- get_print_fun(.step)
  
  if(!is.null(print_fun)) {
    print_fun(.step, width = width, ...)
    return()
  }
  
  pkgs <- unique(c("recipes", generics::required_pkgs(.step)))
  
  for(pkg in pkgs) {
    print_fun <- get_print_fun(.step, rlang::ns_env(pkg))
    
    if(!is.null(print_fun)) {
      print_fun(.step, width = width, ...)
      return()
    }
  }
  
  print_fun <- get_print_fun_anywhere(.step)
  
  if(!is.null(print_fun)) {
    print_fun(.step, width = width, ...)
    return()
  }
  
  cli::cli_abort("Print method not found for {.fun {class(.step)[1]}}")
}

get_print_fun <- function(.step, env = NULL) {
  print_fun <- NULL
  
  for (cl in class(.step)) {
    if((grepl("step", cl, fixed = TRUE) || grepl("check", cl, fixed = TRUE)) && !cl %in% c("step", "check")) {
      if(is.null(env)) {
        print_fun <- utils::getS3method("print", cl, optional = TRUE)
      } else {
        print_fun <- utils::getS3method("print", cl, optional = TRUE, envir = env)
      }
    }
  }
  
  print_fun
}

get_print_fun_anywhere <- function(.step) {
  print_fun <- NULL
  
  for (cl in class(.step)) {
    if((grepl("step", cl, fixed = TRUE) || grepl("check", cl, fixed = TRUE)) && !cl %in% c("step", "check")) {
      print_fun <- utils::getAnywhere(paste0("print.", cl))
    }
  }
  
  print_fun
}

flatten_steps <- function(steps) {
  purrr::list_flatten(purrr::map(steps, flatten_step))
}

flatten_step <- function(x) {
  if(inherits(x, "step_grouped")) {
    x$inner_steps
  } else {
    list(x)
  }
}

bake_nested_steps <- function(.data, .steps, .group, ...) {
  grouped_df <- dplyr::group_by(.data, !!!rlang::syms(.group))
  
  order <- get_grouped_order(grouped_df)
  
  purrr::reduce(
    .steps, .init = grouped_df,
    bake_nested_step, order = order, ...
  )
}

prep_nested_steps <- function(.steps, .group, .data, info = NULL, ...) {
  grouped_df <- dplyr::group_by(.data, !!!rlang::syms(.group))
  
  order <- get_grouped_order(grouped_df)
  
  fits <- vector("list", length = length(.steps))

  for(a in seq_along(.steps)) {
    step <- .steps[[a]]
    
    fit <- prep_nested_step(step, grouped_df, info, ...)
    
    fits[[a]] <- fit
    
    grouped_df <- bake_nested_step(grouped_df, fit, order)
  }
  
  return(list(grouped_df, fits))
}

prep_nested_step <- function(.step, .df, info, ...) {
  dplyr::group_map(
    .df, .keep = TRUE,
    function(x, y) recipes::prep(.step, x, info = info, ...)
  )
}

bake_nested_step <- function(.df, .steps, .order, ...) {
  data <- dplyr::group_split(.df)
  
  results <- purrr::map2(.steps, data, recipes::bake, ...)
  
  dplyr::bind_rows(results)[.order, ]
}

get_grouped_order <- function(data) {
  nested_data <- dplyr::ungroup(tidyr::nest(data))
  
  template_names <-
    colnames(nested_data)[colnames(nested_data) != "data"]
  template <- nested_data[, template_names]
  order_var <- get_name(".order", colnames(template))
  template[[order_var]] <- seq_len(nrow(template))
  
  order <- order(order(dplyr::left_join(
    dplyr::ungroup(data),
    template,
    by = template_names
  )[[order_var]]))
}

is_grouping_step <- function(x) {
  inherits(x, c("step_group_by", "step_ungroup"))
}

add_group_variables <- function(x, step) {
  if(inherits(step, "step_group_by")) {
    unique(c(x, step$names))
  } else {
    x[!x %in% step$names]
  }
}

#' @export
required_pkgs.step_grouped <- function(...) {
  c("nestedmodels", "tidyr", "dplyr", "purrr")
}
