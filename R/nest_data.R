nest_data <- function(data, inner_names, outer_names) {
  nested <- all(!inner_names %in% colnames(data))
  
  if(nested) {
    nested_col <- find_nested_column_with_names(data, inner_names)
    nested_data <- data
    unnested_data <- tidyr::unnest(.data[[nested_col]])
  } else {
    if(dplyr::is_grouped_df(data)) {
      nested_data <- tidyr::nest(data)
      unnested_data <- dplyr::ungroup(data)
    } else {
      to_nest <- inner_names[inner_names %in% colnames(data)]
      unnested_data <- data
      nested_data <- tidyr::nest(data = -c(rlang::ensyms(to_nest)))
      nested_col <- "data"
    }
  }
  
  list(nested_data = nested_data, 
       unnested_data = unnested_data,
       column <- nested_col)
}

find_nested_column_with_names <- function(data, names) {
  list_columns <- purrr::map_lgl(data, is.list)
  if(any(list_columns)) {
    data <- data[,list_columns]
    df_cols <- purrr::map_lgl(data, purrr::some, is.data.frame)
    if(any(df_cols)) {
      if(length(which(df_cols)) == 1) {
        return(colnames(data)[df_cols])
      }
      data <- data[,df_cols]
      cols_valid <- purrr::map(data, ~ {
        names %in% colnames(purrr::compact(.)[[1]])
      })
      valid_df_cols <- purrr::map_lgl(cols_valid, all)
      if(any(valid_df_cols)) {
        if(length(which_valid) == 1) {
          return(colnames(data)[which_valid])
        } else {
          data <- data[,valid_df_cols]
        }
      } else {
        if(any(purrr::map_lgl(cols_valid, any))) {
          n_cols_valid <- purrr::map_int(cols_valid, sum)
          best <- n_cols_valid == max(n_cols_valid)
          if(length(which(best)) == 1) {
            colname <- colnames(data)[best]
            warn_ambiguous_column(colname)
            return(colname)
          } else {
            data <- data[,best]
          }
        }
      }
    } else {
      stop_not_nested()
    }
  } else {
    stop_not_nested()
  }
  # If no error + no column decided on
  index <- which.max(purrr::map_int(data, ~ {
    sum(purrr::map_lgl(., is.data.frame))
  }))
  colname <- colnames(data)[index]
  warn_ambiguous_column(colname)
  return(colname)
}

warn_ambiguous_column <- function(name) {
  cli::cli_warn(c(
    "Ambiguous column to unnest in {.arg new_data}.",
    "Using column {.val {name}}."
  ))
}

stop_not_nested <- function() {
  cli::cli_abort(c(
    "{.arg new_data} is not nested.",
    "i" = "Try manually nesting the data with {.fun tidyr::nest}."
  ))
}
