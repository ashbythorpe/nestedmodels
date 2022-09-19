#' Create splits on nested data
#'
#' `nested_resamples` allows you to use any rsample split function on nested 
#' data.
#' 
#' @param data A data frame.
#' @param inside An expression for the type of resampling to be conducted within
#' each nest. Passed into [rsample::nested_cv()].
#' @param nesting_method A recipe, workflow or NULL.
#' 
#' @details 
#' `nested_resamples` is a wrapper around [rsample::nested_cv()], where the
#' outside resamples are defined by the nested data frames. This means that if
#' the inside resampling method reliably produces 10 splits, you will end up
#' with (10 * the number of nests) splits. This function has been modified to
#' work with [rsample::initial_split()] and similar.
#' 
#' If `nesting_method` is NULL, `data` is assumed to already be nested, and
#' `nested_resamples` will try to find a column that contains nested data 
#' frames.
#' If `nesting_method` is NULL and `data` is grouped (using 
#' [dplyr::group_by()]), the data will be nested (see [tidyr::nest()]) for how
#' this works).
#' If `nesting_method` is a workflow or recipe, and the recipe has a step created using 
#' [step_nest()], `data` will be nested using the step in the recipe.
#' 
#' @seealso [nested_initial_split()] for initial splits.
#' [rsample::nested_cv()] for how `inside` should be formatted.
#' 
#' @examples 
#' nested_data <- example_nested_data %>%
#'   tidyr::nest(-id)
#' 
#' grouped_data <- example_nested_data %>%
#'   dplyr::group_by(id)
#' 
#' recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
#'   step_nest(-id)
#' 
#' wf <- workflows::workflow() %>%
#'   workflows::add_recipe(recipe)
#' 
#' nested_resamples(nested_data, rsample::vfold_cv())
#' 
#' nested_resamples(grouped_data, rsample::mc_cv(times = 10))
#' 
#' nested_resamples(example_nested_data, rsample::initial_split(), 
#'                  nesting_method = recipe)
#'                  
#' nested_resamples(example_nested_data, rsample::initial_time_split(prop = 0.6), 
#'                  nesting_method = workflow)
#' 
#' @export
nested_resamples <- function(data, resamples, nesting_method = NULL, 
                             size_action = c("truncate", "combine", 
                                             "combine-random", "recycle", 
                                             "error"),
                             ...) {
  quo <- rlang::enquo(resamples)
  cl <- rlang::call_match()$resamples
  env <- rlang::caller_env()
  
  size_action <- rlang::arg_match(size_action)
  
  resamples <- get_resamples(quo, cl)
  
  nested_data <- nest_data(data, nesting_method)
  
  colname <- get_colname(data, nesting_method)
  
  nested_colname <- find_nested_column(nested_data, colname)
  
  unnested_data <- tidyr::unnest(nested_data, dplyr::all_of(nested_colname))
  
  actual_resamples <- create_resamples(nested_data[[nested_colname]], 
                                       unnested_data, resamples, size_action,
                                       env, ...)
}

get_resamples <- function(quo, call) {
  expr_str <- rlang::expr_deparse(rlang::get_expr(quo), width = Inf) %>%
    stringr::str_remove_all(" ")
  stringr::str_detect("(", "\\(")
  if(!stringr::str_starts(expr_str, "~") && 
     stringr::str_detect(expr_str, "\\(") &&
     stringr::str_detect(expr_str, "\\)")) {
    call
  } else {
    res <- rlang::eval_tidy(quo)
    if(rlang::is_formula(res)) {
      rlang::as_function(res)
    } else {
      res
    }
  }
}

create_resamples <- function(x, data, resamples, action, env, ...) {
  actual_resamples <- purrr::map(x, eval_resamples, resamples, env, ...)
  if(!is.data.frame(actual_resamples[[1]])) {
    combine_rsets(actual_resamples, x = x)
  } else {
    match_sizes(actual_resamples, action)
    combine_resamples(resamples)
  }
}

c("truncate", "combine", "combine-end",
  "combine-random", "recycle", 
  "recycle-random",
  "error")

match_sizes <- function(actual_resamples, method) {
  l <- purrr::map_int(actual_resamples, nrow)
  if(method == "truncate") {
    purrr::map(actual_resamples, res_truncate, min(l))
  } else if(method == "combine") {
    purrr::map(actual_resamples, res_combine, min(l))
  } else if(method == "combine-end") {
    purrr::map(actual_resamples, res_combine_end, min(l))
  } else if(method == "combine-random") {
    purrr::map(actual_resamples, res_combine_random, min(l))
  } else if(method == "recycle") {
    purrr::map(actual_resamples, res_recycle, max(l))
  } else if(method == "recycle-random") {
    purrr::map(actual_resamples, res_recycle_random, max(l))
  } else {
    cli::cli_abort(c(
      "{.arg resamples} produced different numbers of resamples."
    ))
  }
}

res_truncate <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  preserve_attributes(res[1:len,], res)
}

res_combine <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  res_len <- nrow(res)
  combine_with <- rep(seq_len(len), length.out = res_len - len)
  combine_indexes <- purrr::map(seq_len(len), 
                                ~ which(combine_with == .) + len) %>%
    purrr::flatten_int()
  to_combine <- purrr::map2(seq_len(len), combine_indexes, ~ {
    res$splits[c(.x, .y)]
  })
  
  lres <- res_truncate(res, len)
  print(x)
  lres$splits <- purrr::map(to_combine, combine_same_rsets)
  lres
}

res_combine_end <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  combine_indexes <- (len+1L):res_len
  to_combine <- res$splits[combine_indexes]
  
  lres <- res_truncate(res, len)
  lres$splits[[len]] <- combine_same_rsets(to_combine)
  lres
}

res_combine_random <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  combine_with <- c(
    safe_sample(rep(seq_len(len), res_len %/% (len - 1L))),
    safe_sample(seq_len(len)[seq_len(res_len %% len)])
  )
  combine_indexes <- purrr::map(seq_len(len), 
                                ~ which(combine_with == .) + len) %>%
    purrr::flatten_int()
  to_combine <- purrr::map2(seq_len(len), combine_indexes, ~ {
    res$splits[c(.x, .y)]
  })
  
  lres <- res_truncate(res, len)
  lres$splits <- purrr::map(to_combine, combine_same_rsets)
  lres
}

res_recycle <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  copy_of <- rep(seq_len(res_len), length.out <- len-res_len)
  copy_indexes <- purrr::map(seq_len(res_len), 
                             ~ which(copy_of == .) + res_len) %>%
    purrr::flatten_int()
  to_copy <- purrr::map(copy_indexes, ~ {
    res$splits[.]
  })
  
  lres <- res_extend(res, len)
  lres$splits[res_len + 1,] <- to_copy
  lres
}

res_recycle_random <- function(res, len) {
  if(nrow(res) == len) {
    return(res)
  }
  
  res_len <- nrow(res)
  copy_of <- c(
    safe_sample(rep(seq_len(res_len), len %/% (res_len - 1L))),
    safe_sample(seq_len(res_len)[seq_len(len %% res_len)])
  )
  copy_indexes <- purrr::map(seq_len(res_len), 
                             ~ which(copy_of == .) + res_len) %>%
    purrr::flatten_int()
  to_copy <- purrr::map(copy_indexes, ~ {
    res$splits[.]
  })
  
  lres <- res_extend(res, len)
  lres$splits[res_len + 1,] <- to_copy
  lres
}

res_extend <- function(res, len) {
  extended_res <- tibble::add_row(splits = rep(NA, len - nrow(res)))
  id_prefix <- stringr::str_extract(res$id[1], "^[:alpha:]+")
  extended_res$id <- recipes::names0(len, prefix = id_prefix)
}

safe_sample <- function(x, ...) {
  if(length(x == 1)) {
    x
  } else {
    sample(x, ...)
  }
}

combine_same_rsets <- function(splits) {
  full_splits <- purrr::map(splits, rsample::populate)
  
  data <- full_splits[[1]]$data
  
  split_indexes <- full_splits %>%
    purrr::map(~ {list(.$in_id, .$out_id)})
  
  split_indexes %>%
    purrr::transpose() %>%
    purrr::map(combine_indices) %>%
    purrr::set_names() %>%
    rlang::set_names("analysis", "assessment") %>%
    rsample::make_splits(data = data) %>%
    preserve_attributes(splits[[1]])
}

combine_indices <- function(list) {
  purrr::flatten_int(list) %>%
    unique()
}

eval_resamples <- function(data, resamples, .env, ...) {
  if(rlang::is_call(resamples)) {
    rlang::call_modify(resamples, data = data) %>%
      rlang::eval_tidy(env = env)
  } else if(is.function(resamples)) {
    rlang::exec(resamples, data = data, ..., .env = .env)
  } else if(is.vector(resamples) && is.character(resamples)) {
    if(resamples %in% rsample_fns && rlang::is_installed("rsample") &&
       !rlang::is_attached("rsample")){
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

new_nested_rset <- function(data, column, inside) {
  cl <- rlang::call_match()[["inside"]]
  nrows <- purrr::map_int(column, nrow)
  c_nrows <- c(0L, cumsum(nrows))
  
  outside <- purrr::map2(head(c_nrows, -1), tail(c_nrows, -1), ~ (.x+1):.y) %>%
    purrr::map(~ list(analysis = .)) %>%
    purrr::map(rsample::make_splits, data = data) %>%
    rsample::manual_rset(ids = glue::glue("Nest {1:length(.)}"))
  
  splits <- rlang::inject(rsample::nested_cv(data, outside, !!cl))
  
  splits %>%
    dplyr::select(-.data$splits) %>%
    dplyr::rename(nest_id = "id") %>%
    tidyr::unnest(.data$inner_resamples) %>%
    dplyr::relocate(.data$splits) %>%
    preserve_attributes(splits) %>%
    new_nested_resamples()
}

nest_data <- function(data, nesting_method = NULL) {
  if(is.null(nesting_method)) {
    if(dplyr::is_grouped_df(data)) {
      tidyr::nest(data) %>%
        dplyr::ungroup()
    } else {
      data
    }
  } else if(inherits(nesting_method, "recipe")) {
    apply_nested_recipe(data, get_nested_recipe(nesting_method))
  } else if(inherits(nesting_method, "workflow")) {
    nesting_method <- nesting_method$pre$actions$recipe$recipe
    apply_nested_recipe(data, get_nested_recipe(nesting_method))
  }
}

get_colname <- function(data, nesting_method) {
  if(dplyr::is_grouped_df(data) || 
     inherits(nesting_method, c("recipe", "workflow"))) {
    "data"
  } else {
    NULL
  }
}

find_nested_column <- function(data, colname = NULL) {
  if(!is.null(colname)) {
    return("data")
  }
  list_columns <- purrr::map_lgl(data, is.list)
  if(length(which(list_columns)) == 0) {
    cli::cli_abort(c(
      "{.arg data} deos not have a nested column.",
      "x" = "{.arg nesting_method} was not supplied."
    ))
  }
  maybe_nested <- data[,list_columns]
  df_null_columns <- purrr::map(maybe_nested, purrr::map_lgl, 
                                ~ is.data.frame(.) || is.null(.))
  df_columns <- purrr::map_lgl(maybe_nested, purrr::some, is.data.frame)
  nested_columns <- purrr::map_lgl(df_null_columns, all) & df_columns
  if(length(which(nested_columns)) == 0) {
    nested_columns <- df_columns
    if(length(which(nested_columns)) == 0) {
      cli::cli_abort(c(
        "{.arg data} does not have a nested column.",
        "x" = "{.arg nesting_method} was not supplied."
      ))
    }
  }
  nested_colnames <- colnames(data)[nested_columns]
  if(length(nested_colnames) == 1) {
    nested_colnames
  } else if("data" %in% nested_colnames) {
    "data"
  } else {
    n_dfs <- purrr::map_int(data[,nested_colnames], 
                            ~ {sum(purrr::map_lgl(., is.data.frame))})
    best_column <- which.max(n_dfs)
    nested_colname <- nested_colnames[best_column]
    cli::cli_warn(c(
      "{.arg data} has more than 1 nested column.",
      "i" = "Using column {.val {nested_colname}}."
    ))
    nested_colname
  }
}
