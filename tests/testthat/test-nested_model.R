test_that("nested_model works", {
  skip_if_not_installed("glmnet")
  
  normal_model <- parsnip::linear_reg(penalty = hardhat::tune()) %>%
    parsnip::set_engine("glmnet")
  nested_model <- nested_model("regression", normal_model, allow_par = FALSE, pkgs = NULL)

  expect_equal(
    generics::tunable(normal_model) %>% dplyr::select(-"component"),
    generics::tunable(nested_model) %>% dplyr::select(-"component")
  )
  expect_equal(
    generics::tune_args(normal_model) %>%
      dplyr::select(-"component"),
    generics::tune_args(nested_model) %>%
      dplyr::select(-"component")
  )

  expect_equal(
    parsnip::translate(nested_model),
    parsnip::translate(normal_model)
  )
  
  expect_snapshot({
    print(nested_model)
  })
})
