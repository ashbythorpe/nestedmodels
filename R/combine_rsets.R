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
  full_splits <- purrr::map(splits, rsample::populate)

  format <- splits[[format_index]]

  nrows <- purrr::map_int(x, nrow)
  c_nrows <- c(0L, cumsum(nrows))[-(length(nrows) + 1)]

  split_indexes <- full_splits %>%
    purrr::map(~ {
      list(.$in_id, .$out_id)
    })

  res <- split_indexes %>%
    purrr::map2(c_nrows, add_to_splits) %>%
    transpose_version() %>%
    purrr::map(~ {
      rlang::inject(c(!!!.))
    }) %>%
    rlang::set_names("analysis", "assessment") %>%
    rsample::make_splits(data = data)

  res$id <- format$id
  class(res) <- class(format)
  res
}

#' @noRd
add_to_splits <- function(split, x) {
  split[[1]] <- split[[1]] + x
  split[[2]] <- split[[2]] + x
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
