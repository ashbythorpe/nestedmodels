combine_rsets <- function(splits, data) {
  full_splits <- purrr::map(splits, rsample::populate)
  
  nrows <- purrr::map_int(data, nrow)
  c_nrows <- c(0L, cumsum(nrows))[-(length(nrows)+1)]
  
  split_indexes <- full_splits %>%
    purrr::map(~ {list(.$in_id, .$out_id)})
  
  split_indexes %>%
    purrr::map2(c_nrows, add_to_splits) %>%
    purrr::transpose() %>%
    purrr::map(~ {rlang::inject(c(!!!.))}) %>%
    rlang::set_names("analysis", "assessment") %>%
    rsample::make_splits(data = data) %>%
    preserve_attributes(splits[[1]])
}

add_to_splits <- function(split, x) {
  split[[1]] <- split[[1]] + x
  split[[2]] <- split[[2]] + x
  split
}
