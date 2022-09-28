combine_rsets <- function(splits, x, data, format_index) {
  full_splits <- purrr::map(splits, rsample::populate)

  format <- splits[[format_index]]

  nrows <- purrr::map_int(x, nrow)
  c_nrows <- c(0L, cumsum(nrows))[- (length(nrows) + 1)]

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

add_to_splits <- function(split, x) {
  split[[1]] <- split[[1]] + x
  split[[2]] <- split[[2]] + x
  split
}

# nocov start
transpose_version <- function(x, ...) {
  if (exists("list_transpose", rlang::ns_env("purrr"))) {
    rlang::ns_env("purrrr")$list_transpose(x, ..., simplify = FALSE)
  } else {
    purrr::transpose(x, ...)
  }
}
# nocov end