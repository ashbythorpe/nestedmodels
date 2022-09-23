combine_nested_cv <- function(resamples, data, x, action) {
  res <- match_sizes(resamples, "truncate")
  format <- res[[1]][[res[[2]]]]
  splits <- purrr::map(res[[1]], "splits") %>%
    purrr::pmap( ~ {combine_rsets(list(...), data = data, x = x)})
  
  dfs <- purrr::map(splits, rsample::training)
  inner_res <- purrr::map(res[[1]], "inner_resamples") %>%
    transpose_version()
  
  matched_inner_res <- purrr::map(inner_res, match_sizes, action)
  formats <- purrr::map(matched_inner_res, ~ {.[[1]][[.[[2]]]]})
  inner <- purrr::map(matched_inner_res, 1)
  
  args <- list(
    splits = inner,
    data = dfs,
    format = formats
  )
  
  final_inner <- purrr::pmap(args, combine_nested_cvs)
  new_nested_rset(splits, format = format, inner_resamples = final_inner)
}

combine_nested_cvs <- function(splits, data, x, format) {
  x <- purrr::map(splits, list("splits", 1, "data"))
  purrr::map(splits, "splits") %>%
    purrr::pmap( ~ {combine_rsets(list(...), data = data, x = x)}) %>%
    new_nested_rset(format = format)
}
