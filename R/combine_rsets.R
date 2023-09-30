#' Combine a list of rsplit objects
#'
#' Combine a list of `rsplit` objects into a single `rsplit` object.
#'
#' @param splits A list of `rsplit` objects.
#' @param x A list of data frames, where each item of `splits` corresponds
#'   to each data frame (meaning that each rsplit was created using its
#'   corresponding data frame).
#' @param data The unnested data. `x` should be a column of `data` after
#'   it has been nested.
#' @param format_index The index of the item of `splits` that acts as the
#'   split 'format'. The final result will inherit its class and 'id' from
#'   this object.
#'
#' @returns A single `rsplit` object.
#'
#' @noRd
combine_rsets <- function(splits, x, data, format_index) {
  format <- splits[[format_index]]

  if (inherits(format, "three_way_split")) {
    id_names <- c("train_id", "val_id", "test_id")
  } else {
    id_names <- c("in_id", "out_id")
  }

  full_splits <- purrr::map(splits, populate_all)

  nrows <- purrr::map_int(x, nrow)
  c_nrows <- c(0L, cumsum(nrows))[-(length(nrows) + 1)]

  split_indexes <- full_splits %>%
    purrr::map(function(x) {
      purrr::map(id_names, function(y) x[[y]])
    })

  res <- split_indexes %>%
    purrr::map2(c_nrows, add_to_splits) %>%
    transpose_version() %>%
    purrr::map(~ {
      rlang::inject(c(!!!.))
    })

  for (i in seq_along(id_names)) {
    format[[id_names[i]]] <- res[[i]]
  }

  format$data <- data
  format
}

#' @noRd
add_to_splits <- function(split, x) {
  for (i in seq_along(split)) {
    split[[i]] <- split[[i]] + x
  }
  split
}

# nocov start
#' @noRd
transpose_version <- function(x, ...) {
  if (exists("list_transpose", rlang::ns_env("purrr"))) {
    rlang::ns_env("purrr")$list_transpose(x, ..., simplify = FALSE)
  } else {
    purrr::transpose(x, ...)
  }
}
# nocov end
