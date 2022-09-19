test_that("nested_resamples works", {
  nested_resamples(
    example_nested_data %>% tidyr::nest(data = -id), 
    rsample::initial_split()
  ) %>%
    unclass()
    expect_equal(
      rsample::initial_split(example_nested_data, strata = id) %>%
        unclass()
    )
})

nested_data <- example_nested_data %>%
  tidyr::nest(data = -id)

c_nrows
outside <- purrr::map2(head(c_nrows, -1), tail(c_nrows, -1), ~ (.x+1):.y) %>%
  purrr::map(~ list(analysis = .)) %>%
  purrr::map(rsample::make_splits, data = example_nested_data) %>%
  rsample::manual_rset(ids = glue::glue("Nest {1:length(.)}"))

resamples <- rsample::nested_cv(example_nested_data, outside, rsample::vfold_cv()) %>%
  dplyr::select(-splits) %>%
  dplyr::rename(nest_id = "id") %>%
  tidyr::unnest(inner_resamples) %>%
  dplyr::relocate(splits)

rsample::nested_cv(example_nested_data, outside, rsample::initial_split())

nrows <- purrr::map_int(nested_data$data, nrow)
c_nrows <- c(0L, cumsum(nrows))
splits <- purrr::map(nested_data$data, rsample::initial_split) %>%
  purrr::map(~ {list(.$in_id, .$out_id)})
c_nrows


new_split <- splits %>%
  purrr::map2(nrows, fill_out) %>%
  purrr::map2(c_nrows, add_to_splits) %>%
  purrr::transpose() %>%
  purrr::map(~ {rlang::inject(c(!!!.))}) %>%
  rlang::set_names("analysis", "assessment") %>%
  rsample::make_splits(data = example_nested_data)
new_split
add_to_splits <- function(split, x) {
  split[[1]] <- split[[1]] + x
  split[[2]] <- split[[2]] + x
  split
}
typeof(nrows)
new_split[[1]][[1]] %>% typeof()


resamples <- rsample::vfold_cv(example_nested_data)
resamples
res_truncate(resamples, 5L)
res_combine(resamples, 4L)
res_combine_end(resamples, 4L)
res_combine_random(resamples, 4L)
res_recycle(resamples, 40L)
res_recycle_random(resamples, 40L)

resamples <- nested_resamples(example_nested_data %>% tidyr::nest(data = -id), 
                              rsample::initial_split())
safe_sample(1:5)
sample(1L:5L, size = 5L)
