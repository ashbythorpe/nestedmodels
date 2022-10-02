test_that("augment.nested_model_fit works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -id)

  fit <- fit(model, z ~ x + y + a + b, nested_data)

  expect_equal(
    augment(fit, example_nested_data),
    augment(fit, nested_data)
  )

  expect_equal(
    nrow(augment(fit, example_nested_data %>%
      dplyr::filter(.data$id %in% c(9, 10, 11)))), 150
  )

  invalid_data <- example_nested_data
  invalid_data$id <- NULL

  expect_error(augment(fit, invalid_data))

  invalid_data$id <- c(rep(5L, 500), rep(11L, 500))

  fit$fit$.model_fit[3] <- list(NULL)

  expect_warning(augment(fit, example_nested_data))
  expect_equal(
    nrow(suppressWarnings(augment(fit, example_nested_data))),
    nrow(example_nested_data)
  )
})

test_that("fix_augmented_predictions works", {
  expect_error(fix_augmented_predictions(list(
    NULL, NULL, NULL
  ), list(1, 1, 1)))

  expect_warning(fix_augmented_predictions(list(
    tibble::tibble(.pred = 1, .pred2 = 2),
    NULL,
    "a"
  ), data = list(
    1,
    tibble::tibble(x = 1:10),
    1
  )))

  expect_equal(suppressWarnings(fix_augmented_predictions(list(
    tibble::tibble(x = 1, .pred = 1, .pred2 = 2),
    NULL,
    "a"
  ), data = list(
    1,
    tibble::tibble(x = 1:10),
    1
  ))), list(
    tibble::tibble(x = 1, .pred = 1, .pred2 = 2),
    tibble::tibble(x = 1:10, .pred = NA, .pred2 = NA),
    "a"
  ))
})
