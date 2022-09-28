test_that("example_nested_data is valid", {
  expect_lt(
    length(unique(example_nested_data$id)),
    nrow(example_nested_data) / 10
  )
})
