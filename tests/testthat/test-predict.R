test_that("predict works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -id)

  fit <- fit(model, z ~ x + y + a + b, nested_data)

  expect_equal(
    predict(fit, example_nested_data),
    predict(fit, nested_data)
  )

  expect_equal(
    predict(fit, example_nested_data %>%
      dplyr::filter(id == 10)),
    predict(fit, example_nested_data)[
      which(example_nested_data$id == 10),
    ]
  )

  invalid_data <- example_nested_data
  invalid_data$id <- NULL

  expect_error(predict(fit, invalid_data))

  invalid_data$id <- c(rep(5L, 500), rep(11L, 500))

  predict(fit, invalid_data)

  fit$fit$.model_fit[3] <- list(NULL)

  expect_warning(predict(fit, example_nested_data))
  expect_equal(
    nrow(suppressWarnings(predict(fit, example_nested_data))),
    nrow(example_nested_data)
  )
})

test_that("predict_raw.nested_model_spec works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -id)

  fit <- fit(model, z ~ x + y + a + b, nested_data)

  expect_equal(
    length(parsnip::predict_raw(fit, example_nested_data)),
    nrow(example_nested_data)
  )
})

test_that("outer names warnings work", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -c(id, id2))

  fit <- fit(model, z ~ ., nested_data)

  data <- example_nested_data
  data$id <- NULL

  expect_warning(predict(fit, data))
  expect_warning(augment(fit, data))
})


test_that("multivariate predictions work", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  nested_data <- tidyr::nest(example_nested_data, data = -id)

  fit <- fit(model, cbind(z, y) ~ x + a + b, nested_data)

  expect_equal(ncol(predict(fit, example_nested_data)), 2)
  parsnip::predict_raw(fit, example_nested_data)
  augment(fit, example_nested_data)
})

test_that("multivariate workflows work", {
  skip_if_not_installed("workflows")
  
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  recipe <- recipes::recipe(example_nested_data, z + y ~ x + a + b + id) %>%
    step_nest(id)

  wf <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)

  fit <- fit(wf, example_nested_data)

  expect_equal(ncol(predict(fit, example_nested_data)), 2)
})

test_that("fix_predictions works", {
  expect_error(fix_predictions(list(NULL, NULL), list(NULL, NULL)))

  expect_warning(fix_predictions(list(
    tibble::tibble(.pred = 1),
    NULL,
    "a"
  ), data = list(
    1,
    tibble::tibble(.rows = 10),
    1
  )))

  expect_equal(suppressWarnings(fix_predictions(list(
    tibble::tibble(.pred = 1),
    NULL,
    "a"
  ), data = list(
    1,
    tibble::tibble(.rows = 10),
    1
  ))), list(
    tibble::tibble(.pred = 1),
    tibble::tibble(.pred = rep(NA, 10)),
    "a"
  ))

  expect_warning(fix_predictions(list(
    tibble::tibble(.pred = 1, .pred2 = 2),
    NULL,
    NULL
  ), data = list(
    1,
    tibble::tibble(.rows = 10),
    tibble::tibble(.rows = 0)
  )))

  expect_equal(suppressWarnings(fix_predictions(list(
    tibble::tibble(.pred = 1, .pred2 = 2),
    NULL,
    NULL
  ), data = list(
    1,
    tibble::tibble(.rows = 10),
    tibble::tibble(.rows = 0)
  ))), list(
    tibble::tibble(.pred = 1, .pred2 = 2),
    tibble::tibble(.pred = NA, .pred2 = NA, .rows = 10),
    tibble::tibble(.pred = NA, .pred2 = NA, .rows = 0)
  ))

  expect_warning(fix_predictions(list(
    NULL,
    c(1, 2, 3, 4),
    NULL
  ), list(
    tibble::tibble(.rows = 4),
    1,
    tibble::tibble(.rows = 11)
  )))

  expect_equal(suppressWarnings(fix_predictions(list(
    NULL,
    c(1, 2, 3, 4),
    NULL
  ), list(
    tibble::tibble(.rows = 4),
    1,
    tibble::tibble(.rows = 11)
  ))), list(
    rep(NA, 4),
    1:4,
    rep(NA, 11)
  ))

  expect_warning(fix_predictions(list(
    NULL,
    NULL,
    matrix(ncol = 2, dimnames = list(NULL, c(".pred", ".pred2")))
  ), list(
    tibble::tibble(.rows = 0),
    tibble::tibble(.rows = 100),
    1
  )))

  expect_equal(suppressWarnings(fix_predictions(list(
    NULL,
    NULL,
    matrix(ncol = 2, dimnames = list(NULL, c(".pred", ".pred2")))
  ), list(
    tibble::tibble(.rows = 0),
    tibble::tibble(.rows = 100),
    1
  ))), list(
    matrix(logical(), ncol = 2, dimnames = list(NULL, c(".pred", ".pred2"))),
    matrix(NA,
      nrow = 100, ncol = 2, dimnames =
        list(NULL, c(".pred", ".pred2"))
    ),
    matrix(ncol = 2, dimnames = list(NULL, c(".pred", ".pred2")))
  ))

  expect_warning(fix_predictions(list(
    NULL,
    NULL,
    matrix(ncol = 2)
  ), list(
    tibble::tibble(.rows = 0),
    tibble::tibble(.rows = 100),
    1
  )))

  expect_equal(suppressWarnings(fix_predictions(list(
    NULL,
    NULL,
    matrix(ncol = 2)
  ), data = list(
    tibble::tibble(.rows = 0),
    tibble::tibble(.rows = 100),
    1
  ))), list(
    matrix(ncol = 2, nrow = 0),
    matrix(ncol = 2, nrow = 100),
    matrix(ncol = 2)
  ))
})
