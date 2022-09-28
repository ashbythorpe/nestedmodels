
get_name <- function(name, colnames) {
  vctrs::vec_as_names(c(name, colnames), repair = "unique", quiet = TRUE)[1]
}

get_nested_step <- function(recipe) {
  index <- get_nested_step_index(recipe)
  recipe$steps[[index]]
}

check_df <- function(x, name) {
  if(!is.data.frame(x)) {
    x <- tryCatch(
      as.data.frame(x),
      error = function(c) stop_bad_type(name, "a data frame", x)
    )
  }
  x
}

nest_data_method <- function(data, nesting_method = NULL) {
  colname <- get_name(".data", colnames(data))
  if(is.null(nesting_method)) {
    if(dplyr::is_grouped_df(data)) {
      group_vars <- dplyr::group_vars(data)
      nested_data <- data %>%
        tidyr::nest(!!colname := -c(!!!rlang::syms(group_vars))) %>%
        dplyr::ungroup()
    } else {
      nested_data <- data
      colname <- find_nested_column(data)
    }
  } else if(inherits(nesting_method, "recipe")) {
    nested_step <- get_nested_step(nesting_method)
    nested_data <- tidyr::nest(example_nested_data, 
                               !!colname := -c(!!!nested_step$terms))
  } else if(inherits(nesting_method, "workflow")) {
    nested_step <- get_nested_step(nesting_method$pre$actions$recipe$recipe)
    nested_data <- tidyr::nest(example_nested_data, 
                               !!colname := -c(!!!nested_step$terms))
  }
  list(
    data = nested_data,
    colname = colname
  )
}

find_nested_column <- function(data) {
  list_columns <- purrr::map_lgl(data, is.list)
  
  if(any(list_columns)) {
    data <- data[,list_columns]
    df_cols <- purrr::map_lgl(data, purrr::some, is.data.frame)
    if(any(df_cols)) {
      if(length(which(df_cols)) == 1) {
        return(colnames(data)[df_cols])
      }
      data <- data[,df_cols]
      index <- which.max(purrr::map_int(data, ~ {
        sum(purrr::map_lgl(., is.data.frame))
      }))
      colname <- colnames(data)[index]
      warn_ambiguous_column(colname)
      return(colname)
    } else {
      stop_not_nested("data")
    }
  } else {
    stop_not_nested("data")
  }
}

get_nested_step_index <- function(recipe) {
  recipe$steps %>%
    purrr::map(class) %>%
    purrr::map_lgl(., ~ {
      "step_nest" %in% .
    }) %>%
    which()
}


remove_class <- function(object, class) {
  class(object) <-
    setdiff(class(object), class)
  object
}

as_ordered_factor <- function(x) {
  forcats::as_factor(x) %>%
    forcats::fct_inorder()
}

# copied from the rlang package
# `%||%` <- function(x, y) {
#   if (is.null(x)) {
#     y
#   } else {
#     x
#   }
# }



