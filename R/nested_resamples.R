#' Create splits with nested data
#'
#' Use any rsample split function on nested data, where nests act as
#' strata. This almost guarantees that every split will contain data from
#' every nested data frame.
#'
#' @param data A data frame.
#' @param resamples An expression, function, formula or string that can
#'   be evaluated to produce an `rset` or `rsplit` object.
#' @param nesting_method A recipe, workflow or `NULL`, used to nest `data`
#'   if `data` is not already nested (see Details).
#' @param size_action If different numbers of splits are produced in each
#'   nest, how should sizes be matched? (see Details)
#' @param ... Extra arguments to pass into `resamples`.
#'
#' @details
#' This function breaks down a data frame into smaller, nested data frames.
#' Resampling is then performed within these nests, and the results are
#' combined together at the end. This ensures that each split contains
#' data from every nest. However, this function does not perform any
#' pooling (unlike [rsample::make_strata()]), so you may run into issues
#' if a nest is too small.
#'
#' # Nesting Data
#'
#' `data` can be nested in several ways:
#' If `nesting_method` is `NULL` and `data` is grouped (using
#' [dplyr::group_by()]), the data will be nested (see [tidyr::nest()]
#' for how this works).
#' If `data` is not grouped, it is assumed to already be nested, and
#' `nested_resamples` will try to find a column that contains nested data
#' frames.
#' If `nesting_method` is a workflow or recipe, and the recipe has a step
#' created using  [step_nest()], `data` will be nested using the step in
#' the recipe. This is convenient if you've already created a recipe or
#' workflow, as it saves a line of code.
#'
#' # Resample Evaluation
#'
#' The `resamples` argument can take many forms:
#'  * A function call, such as `vfold_cv(v = 5)`. This is similar to the
#'   format of [rsample::nested_cv()].
#'  * A function, such as `rsample::vfold_cv`.
#'  * A purrr-style anonymous function, which will be converted to a
#'   function using [rlang::as_function()].
#'  * A string, which will be evaluated using [rlang::exec()].
#'
#' Every method will be evaluated with `data` passed in as the first
#' argument (with name 'data').
#'
#' # Size Matching
#'
#' Before the set of resamples created in each nest can be combined, they
#' must contain the same number of splits. For most resampling methods,
#' this will not be an issue. [rsample::vfold_cv()], for example, reliably
#' creates the number of splits defined in its 'v' argument. However,
#' other resampling methods, like [rsample::rolling_origin()], depend on
#' the size of their 'data' argument, and therefore may produce different
#' numbers of resamples when presented with differently sized nests.
#'
#' The `size_action` argument defines many ways of matching the sizes of
#' resample sets with different numbers of splits. These methods will either try
#' to reduce the number of splits in each set until each rset is the same
#' length as the set with the lowest number of splits; or the opposite,
#' where each rset will have the same number of splits as the largest set.
#'
#' "truncate", the default, means that all splits beyond the required
#' length will be removed.
#'
#' "recycle" means that sets of splits will be extended by repeating
#' elements until the required length has been reached, mimicking the
#' process of vector recycling. The advantage of this method is that all
#' created splits will be preserved.
#'
#' "recycle-random" is a similar process to recycling, but splits will be
#' copied at random to spaces in the output, which may be important if
#' the order of resamples matters. This process is not completely random,
#' and the program makes sure that every split is copied roughly the same
#' number of times.
#'
#' "combine" gets rid of excess splits by combining them with previous ones.
#' This means the training and testing rows are merged into one split.
#' Combining is done systematically: if a set of splits needs to be
#' compacted down to a set of 5, the first split is combined with the
#' sixth split, then the eleventh, then the sixteenth, etc. This approach
#' is not recommended, since it is not clear what the benefit of a
#' combined split is.
#'
#' "combine-random" combines each split with a random set of other splits,
#' instead of the systematic process described in the previous method.
#' Once again, this process is not actually random, and each split will
#' be combined with roughly the same number of other splits.
#'
#' "combine-end" combines every excess split with the last non-excess
#' split.
#'
#' "error" throws an error if each nest does not produce the same number
#' of splits.
#'
#' @seealso [rsample::initial_split()] for an example of the strata
#'  argument.
#'
#' @returns Either an `rsplit` object or an `rset` object, depending on
#' `resamples`.
#'
#' @examples
#' nested_data <- example_nested_data %>%
#'   tidyr::nest(data = -id)
#'
#' grouped_data <- example_nested_data %>%
#'   dplyr::group_by(id)
#'
#' recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
#'   step_nest(id)
#'
#' wf <- workflows::workflow() %>%
#'   workflows::add_recipe(recipe)
#'
#' nested_resamples(nested_data, rsample::vfold_cv())
#'
#' nested_resamples(
#'   dplyr::group_by(example_nested_data, id),
#'   ~ rsample::initial_split(.)
#' )
#'
#' nested_resamples(example_nested_data, ~ {
#'   rsample::validation_split(.)
#' }, nesting_method = recipe)
#'
#' nested_resamples(example_nested_data, rsample::bootstraps,
#'   times = 25, nesting_method = wf
#' )
#'
#' # nested nested resamples
#'
#' nested_resamples(nested_data, rsample::nested_cv(
#'   rsample::vfold_cv(),
#'   rsample::bootstraps()
#' ))
#'
#' @export
nested_resamples <- function(data, resamples, nesting_method = NULL,
                             size_action = c(
                               "truncate", "recycle",
                               "recycle-random", "combine",
                               "combine-random", "combine-end",
                               "error"
                             ),
                             ...) {
  quo <- rlang::enquo(resamples)
  cl <- rlang::call_match()$resamples
  env <- rlang::caller_env()

  if (!is.data.frame(data)) {
    stop_bad_type("data", "a data frame", data)
  }

  if (!is.null(nesting_method) &&
    !inherits(nesting_method, c("recipe", "workflow"))) {
    stop_bad_type(
      "nesting_method", "a recipe, workflow or NULL",
      nesting_method
    )
  }

  size_action <- rlang::arg_match(size_action)
  resamples <- get_resamples(quo, cl)

  nest_data_res <- nest_data_method(data, nesting_method)
  nested_data <- nest_data_res$data
  nested_colname <- nest_data_res$colname

  unnested_data <- tidyr::unnest(nested_data, tidyselect::all_of(nested_colname))

  create_resamples(
    nested_data[[nested_colname]],
    unnested_data, resamples, size_action,
    env, ...
  )
}

get_resamples <- function(quo, call) {
  expr_str <- rlang::expr_deparse(rlang::get_expr(quo), width = Inf) %>%
    stringr::str_remove_all(" ") %>%
    paste0(collapse = "")
  if (!stringr::str_starts(expr_str, "~") &&
    stringr::str_detect(expr_str, "\\(") &&
    stringr::str_detect(expr_str, "\\)")) {
    call
  } else {
    res <- rlang::eval_tidy(quo)
    if (rlang::is_formula(res)) {
      rlang::as_function(res)
    } else {
      res
    }
  }
}

create_resamples <- function(x, data, resamples, action, env, ...) {
  actual_resamples <- purrr::map(x, eval_resamples, resamples, env, ...)
  if (inherits(actual_resamples[[1]], "nested_cv")) {
    combine_nested_cv(actual_resamples, data = data, x = x, action = action)
  } else if (!is.data.frame(actual_resamples[[1]])) {
    combine_rsets(actual_resamples, x = x, data = data, format_index = 1)
  } else {
    res <- match_sizes(actual_resamples, action)
    format <- res$rset[[res$index]]
    purrr::map(res$rset, "splits") %>%
      purrr::pmap(~ {
        combine_rsets(list(...),
          x = x, data = data,
          format_index = res$index
        )
      }) %>%
      new_nested_rset(format = format)
  }
}

match_sizes <- function(actual_resamples, method) {
  l <- purrr::map_int(actual_resamples, nrow)
  if (method == "truncate") {
    list(
      rset = purrr::map(actual_resamples, res_truncate, min(l)),
      index = which.min(l)
    )
  } else if (method == "combine") {
    list(
      rset = purrr::map(actual_resamples, res_combine, min(l)),
      index = which.min(l)
    )
  } else if (method == "combine-end") {
    list(
      rset = purrr::map(actual_resamples, res_combine_end, min(l)),
      index = which.min(l)
    )
  } else if (method == "combine-random") {
    list(
      rset = purrr::map(actual_resamples, res_combine_random, min(l)),
      index = which.min(l)
    )
  } else if (method == "recycle") {
    list(
      rset = purrr::map(actual_resamples, res_recycle, max(l)),
      index = which.max(l)
    )
  } else if (method == "recycle-random") {
    list(
      rset = purrr::map(actual_resamples, res_recycle_random, max(l)),
      index = which.max(l)
    )
  } else {
    cli::cli_abort(c(
      "{.arg resamples} produced different numbers of resamples."
    ))
  }
}

eval_resamples <- function(data, resamples, .env, ...) {
  if (rlang::is_call(resamples)) {
    rlang::call_modify(resamples, data = data, ...) %>%
      rlang::eval_tidy(env = .env)
  } else if (is.function(resamples)) {
    rlang::exec(resamples, data = data, ..., .env = .env)
  } else if (is.vector(resamples) && is.character(resamples)) {
    if (rlang::is_installed("rsample") &&
      resamples %in% ls(rlang::ns_env("rsample")) &&
      !rlang::is_attached("rsample")) {
      rlang::exec(resamples, data = data, ..., .env = rlang::ns_env("rsample"))
    } else {
      rlang::exec(resamples, data = data, ..., .env = .env)
    }
  } else {
    cli::cli_abort(c(
      "{.arg resamples} is not an expression or function."
    ))
  }
}

new_nested_rset <- function(splits, format, ...) {
  extra_columns <- rlang::list2(...)
  format$splits <- splits
  purrr::walk2(names(extra_columns), extra_columns, ~ {
    format[[.x]] <<- .y
  })
  format
}
