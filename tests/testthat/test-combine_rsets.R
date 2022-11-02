test_that("add_to_splits works", {
  expect_equal(add_to_splits(list(12L, 23L), 7L), list(19L, 30L))
})

test_that("combine_rsets works", {
  data <- tibble::tibble(
    id = c(rep(1, 5), rep(2, 5), rep(3, 5)),
    x = 1:15
  )
  x <- tidyr::nest(data, data = -"id")$data
  splits <- purrr::map(x, rsample::initial_split, prop = 4 / 5)
  actual_splits <- rsample::make_splits(
    list(
      analysis = c(
        1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L, 11L,
        12L, 13L, 14L
      ),
      assessment = c(5L, 10L, 15L)
    ),
    data
  )
  res <- combine_rsets(splits, data = data, x = x, format_index = 1)
  expect_equal(actual_splits$data, res$data)
  expect_equal(length(actual_splits$in_id), length(res$in_id))
  expect_equal(length(actual_splits$out_id), length(res$out_id))
  expect_s3_class(res, "rsplit")
})
