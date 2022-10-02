test_that("check_df works", {
  check_df(matrix(c(1, 2, 3, 4, 5))) %>%
    expect_s3_class("data.frame")
})

test_that("find_nested_column works", {
  expect_error(find_nested_column(tibble::tibble(
    x = 1:100
  )), class = "not_nested")

  expect_error(find_nested_column(tibble::tibble(
    x = list(1, 2, 3, 4)
  )), class = "not_nested")

  expect_warning(find_nested_column(tibble::tibble(
    x = list(tibble::tibble(), NULL),
    y = list(tibble::tibble(), data.frame())
  )))

  expect_equal(suppressWarnings(find_nested_column(tibble::tibble(
    x = list(tibble::tibble(), NULL),
    y = list(tibble::tibble(), data.frame())
  ))), "y")
})
