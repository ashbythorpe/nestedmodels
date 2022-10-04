test_that("broom methods work", {
  withr::local_package("broom")

  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()

  fit <- fit(
    model, z ~ x + y + a + b,
    tidyr::nest(example_nested_data, data = -.data$id)
  )
  withr::with_package("broom", {
    tidy(fit)
    expect_equal(nrow(glance(fit)), 1)
    expect_equal(nrow(glance_nested(fit)), nrow(fit$fit))

    expect_error(glance_nested(1), class = "bad_class")
  })
})

test_that("combine_nested_rows works", {
  expect_equal(combine_nested_rows(c(1, 2, 3)), 2)
  expect_equal(combine_nested_rows(c(1L, 1L, 1L)), 1L)
  expect_equal(combine_nested_rows(c("a", "a", "a", "a")), "a")
  expect_equal(combine_nested_rows(list(list(), list())), list(list()))
  expect_equal(combine_nested_rows(c("a", "b", "a")), list(c("a", "b", "a")))
  x <- expect_equal(
    x <- combine_nested_rows(list(list(), list(1))),
    list(list(list(), list(1)))
  )
})
