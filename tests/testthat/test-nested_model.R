test_that("nested_model works", {
  normal_model <- parsnip::linear_reg(penalty = hardhat::tune()) %>%
    parsnip::set_engine("glmnet")
  nested_model <- nested_model("regression", normal_model)

  expect_equal(
    generics::tunable(normal_model) %>% dplyr::select(-.data$component),
    generics::tunable(nested_model) %>% dplyr::select(-.data$component)
  )
  expect_equal(
    generics::tune_args(normal_model) %>%
      dplyr::select(-.data$component),
    generics::tune_args(nested_model) %>%
      dplyr::select(-.data$component)
  )

  expect_equal(
    parsnip::translate(nested_model),
    parsnip::translate(normal_model)
  )

  purrr::quietly(print)(nested_model)$output %>%
    expect_match("Nested Model Specification")
})
