test_that("multi_predict.nested_model_fit works", {
  withr::local_options(warnPartialMatchArgs = FALSE)

  model <- parsnip::linear_reg(penalty = hardhat::tune()) %>%
    parsnip::set_engine("glmnet") %>%
    nested()

  fit <- fit(model, z ~ ., tidyr::nest(example_nested_data, data = -.data$id))

  suppressWarnings(
    pred <- parsnip::multi_predict(fit, example_nested_data,
      penalty = c(1, 0.5, 0.2)
    )
  )

  purrr::map_int(pred$.pred, nrow) %>%
    unique() %>%
    expect_equal(3)

  invalid_data <- example_nested_data
  invalid_data$id <- NULL

  expect_error(parsnip::multi_predict(fit, invalid_data,
    penalty = c(1, 0.5, 0.2)
  ))

  invalid_data$id <- c(rep(5L, 500), rep(11L, 500))

  parsnip::multi_predict(fit, invalid_data, penalty = c(1, 0.5, 0.2))

  fit$fit$.model_fit[3] <- list(NULL)

  expect_warning(parsnip::multi_predict(fit, example_nested_data,
    penalty = c(1, 0.5, 0.2)
  ))
  expect_equal(
    nrow(suppressWarnings(parsnip::multi_predict(fit, example_nested_data,
      penalty = c(1, 0.5, 0.2)
    ))),
    nrow(example_nested_data)
  )
})

test_that("multi_predict outer names warnings work", {
  withr::local_options(warnPartialMatchArgs = FALSE)

  model <- parsnip::linear_reg(penalty = 1) %>%
    parsnip::set_engine("glmnet") %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -c(id, id2))

  fit <- fit(model, z ~ ., nested_data)

  data <- example_nested_data
  data$id <- NULL

  expect_warning(parsnip::multi_predict(fit, data, penalty = c(1, 0.5, 0.2)))
})
