test_that("res_truncate works", {
  resamples <- rsample::vfold_cv(example_nested_data, 10)

  expect_equal(res_truncate(resamples, 10L), resamples)
  expect_equal(res_truncate(resamples, 5L), resamples[1:5, ],
    ignore_attr = TRUE
  )
  expect_equal(res_truncate(resamples, 1L), resamples[1, ],
    ignore_attr = TRUE
  )
})

test_that("res_combine works", {
  resamples <- rsample::vfold_cv(example_nested_data, 10)
  resamples2 <- rsample::rolling_origin(
    example_nested_data,
    initial = 100, assess = 10, cumulative = FALSE, skip = 50
  )

  expect_equal(res_combine(resamples, 10L), resamples)
  expect_equal(res_combine(resamples2, nrow(resamples2)), resamples2)
  expect_equal(nrow(res_combine(resamples, 5L)), 5L)
  expect_equal(nrow(res_combine(resamples, 2L)), 2L)
  expect_equal(nrow(res_combine(resamples, 9L)), 9L)
  expect_equal(nrow(res_combine(resamples2, 16L)), 16L)
  expect_equal(nrow(res_combine(resamples2, 1L)), 1L)
})

test_that("res_combine_end works", {
  resamples <- rsample::vfold_cv(example_nested_data, 10)
  resamples2 <- rsample::rolling_origin(
    example_nested_data,
    initial = 100, assess = 10, cumulative = FALSE, skip = 50
  )

  expect_equal(res_combine_end(resamples, 10L), resamples)
  expect_equal(res_combine_end(resamples2, nrow(resamples2)), resamples2)
  expect_equal(nrow(res_combine_end(resamples, 5L)), 5L)
  expect_equal(nrow(res_combine_end(resamples, 2L)), 2L)
  expect_equal(nrow(res_combine_end(resamples, 9L)), 9L)
  expect_equal(nrow(res_combine_end(resamples2, 16L)), 16L)
  expect_equal(nrow(res_combine_end(resamples2, 1L)), 1L)
})

test_that("res_combine_random works", {
  resamples <- rsample::vfold_cv(example_nested_data, 10)
  resamples2 <- rsample::rolling_origin(
    example_nested_data,
    initial = 100, assess = 10, cumulative = FALSE, skip = 50
  )

  expect_equal(res_combine_random(resamples, 10L), resamples)
  expect_equal(res_combine_random(resamples2, nrow(resamples2)), resamples2)
  expect_equal(nrow(res_combine_random(resamples, 5L)), 5L)
  expect_equal(nrow(res_combine_random(resamples, 2L)), 2L)
  expect_equal(nrow(res_combine_random(resamples, 9L)), 9L)
  expect_equal(nrow(res_combine_random(resamples2, 16L)), 16L)
  expect_equal(nrow(res_combine_random(resamples2, 1L)), 1L)
})

test_that("res_recycle works", {
  resamples <- rsample::vfold_cv(example_nested_data, 10)

  expect_equal(res_recycle(resamples, 10L), resamples)
  expect_equal(nrow(res_recycle(resamples, 11L)), 11L)
  expect_equal(nrow(res_recycle(resamples, 15L)), 15L)
  expect_equal(nrow(res_recycle(resamples, 25L)), 25L)
})

test_that("res_recycle_random works", {
  resamples <- rsample::vfold_cv(example_nested_data, 10)

  expect_equal(res_recycle_random(resamples, 10L), resamples)
  expect_equal(nrow(res_recycle_random(resamples, 11L)), 11L)
  expect_equal(nrow(res_recycle_random(resamples, 15L)), 15L)
  expect_equal(nrow(res_recycle_random(resamples, 25L)), 25L)
})
