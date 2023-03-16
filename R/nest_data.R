#' Create a nested or unnested data frame from a data frame.
#'
#' `nest_data()` takes a data frame, along with the names that should be on
#' the inside and outside of the nested data frames. It figures out whether
#' the data is nested, and nests/unnests the data depending on this
#' condition.
#'
#' @param data A data frame, nested or unnested.
#' @param inner_names The column names that should be on the inside of
#'  the inner data frames of the inner data frames.
#' @param outer_names The column names that should stay on the outside of
#'  the nested data frame.
#'
#' @returns A names list, with 4 items:
#' \describe {
#'   \item{nested_data}{The nested data frame.}
#'   \item{column}{The nested column (that contains the inner data frames.)}
#'   \item{order}{A sequence of numbers, showing how the nested data can be
#'    transformed after being unnested to match the unnested data.}
#' }
#' These values follow the rule that the following is always true (although
#' the column order is sometimes different):
#' ```
#' identical(tidyr::unnest(nested_data, .data[[column]])[order,],
#'           unnested_data)
#' ```
#'
#' @noRd
nest_data <- function(data, inner_names, outer_names) {
  if (identical(outer_names, ".nest_id")) {
    nested_data <- tidyr::nest(data, data = -".nest_id")
    
    template <- nested_data[, ".nest_id"]

    template$.order <- seq_len(nrow(template))
    
    order <- order(order(dplyr::left_join(
      data,
      template,
      by = ".nest_id"
    )[[".order"]]))
    
    return(list(
      nested_data = nested_data,
      column = "data",
      order = order
    ))
  }

  nested <- all(!inner_names %in% colnames(data))

  if (nested) {
    nested_col <- find_nested_column_with_names(data, inner_names)
    nested_data <- data
    
    unnested_data <- tidyr::unnest(data, tidyselect::all_of(nested_col))
    order <- seq_len(nrow(unnested_data))
  } else {
    if (dplyr::is_grouped_df(data)) {
      nested_data <- tidyr::nest(data) %>%
        dplyr::ungroup()
      unnested_data <- dplyr::ungroup(data)
      nested_col <- "data"
      
      template_names <-
        colnames(nested_data)[colnames(nested_data) != "data"]
      template <- nested_data[, template_names]
      
      order_var <- get_name(".order", colnames(template))
      template[[order_var]] <- seq_len(nrow(template))
      
      order <- order(order(dplyr::left_join(
        unnested_data,
        template,
        by = template_names
      )[[order_var]]))
      
      if (!all(outer_names %in% colnames(nested_data))) {
        return(nest_data(dplyr::ungroup(data), inner_names, outer_names))
      }
      
      if (!all(inner_names %in% colnames(nested_data$data[[1]]))) {
        try_2 <- nest_data(dplyr::ungroup(data), inner_names, outer_names)
        
        if (sum(inner_names %in% colnames(nested_data$data[[1]])) <
          sum(inner_names %in% colnames(try_2$nested_data[[
          try_2$column]][[1]]))) {
          return(try_2)
        }
      }
    } else {
      to_nest <- inner_names[inner_names %in% colnames(data)]
      unnested_data <- data
      
      nested_data <- tidyr::nest(data, data = c(!!!rlang::syms(to_nest)))
      nested_col <- "data"
      
      template_names <-
        colnames(nested_data)[colnames(nested_data) != "data"]
      template <- nested_data[, template_names]
      
      order_var <- get_name(".order", colnames(template))
      template[[order_var]] <- seq_len(nrow(template))
      
      order <- order(order(dplyr::left_join(
        unnested_data,
        template,
        by = template_names
      )[[order_var]]))
    }
  }

  list(
    nested_data = nested_data,
    column = nested_col,
    order = order
  )
}

find_nested_column_with_names <- function(data, names) {
  list_columns <- purrr::map_lgl(data, is.list)
  if (any(list_columns)) {
    data <- data[, list_columns]
    df_cols <- purrr::map_lgl(data, purrr::some, is.data.frame)
    if (any(df_cols)) {
      if (length(which(df_cols)) == 1) {
        return(colnames(data)[df_cols])
      }
      data <- data[, df_cols]
      cols_valid <- purrr::map(data, valid_cols, names)
      valid_df_cols <- purrr::map_lgl(cols_valid, all)
      if (any(valid_df_cols)) {
        which_valid <- which(valid_df_cols)
        if (length(which_valid) == 1) {
          return(colnames(data)[which_valid])
        } else {
          data <- data[, valid_df_cols]
        }
      } else {
        if (any(purrr::map_lgl(cols_valid, any))) {
          n_cols_valid <- purrr::map_int(cols_valid, sum)
          best <- n_cols_valid == max(n_cols_valid)
          if (length(which(best)) == 1) {
            colname <- colnames(data)[best]
            warn_ambiguous_column("new_data", colname)
            return(colname)
          } else {
            data <- data[, best]
          }
        }
      }
    } else {
      stop_not_nested("new_data")
    }
  } else {
    stop_not_nested("new_data")
  }
  # If no error + no single column decided on
  index <- which.max(purrr::map_int(data, ~ {
    sum(purrr::map_lgl(., is.data.frame))
  }))
  colname <- colnames(data)[index]
  warn_ambiguous_column("new_data", colname)
  return(colname)
}

valid_cols <- function(data, names) {
  if (length(purrr::compact(data)) != 0) {
    names %in% colnames(purrr::compact(data)[[1]])
  } else {
    FALSE
  }
}
