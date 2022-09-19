test_that("multiplication works", {
  tibble::tibble(x = 1:2, data = list(tibble::tibble(
    y = 1:10,
    z = 11:20
  ), tibble::tibble(
    y = 30:39,
    z = 100:109
  ))) %>%
    nested_resamples(NULL, rsample::vfold_cv, .) %>%
    extract_nested_split(., 1) %>%
    extract_nested_training()
})
