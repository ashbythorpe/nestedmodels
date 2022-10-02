test_that("nested works", {
  expect_error(nested(1), class = "bad_class")
  spec <- parsnip::linear_reg()

  nested_spec <- nested(spec)
  expect_s3_class(nested_spec, c("nested_model", "model_spec"), exact = TRUE)
  expect_equal(nested_spec$eng_args$model_spec[[1]], spec)
  expect_equal(nested(nested_spec), nested_spec)

  workflow <- workflows::workflow() %>%
    workflows::add_model(spec)
  expect_false(is_nested(workflow))
  expect_equal(nested(workflow)$fit$actions$model$spec, nested_spec)
  expect_true(is_nested(nested(workflow)))
  expect_equal(nested(nested(workflow)), nested(workflow))

  expect_true(is_nested(workflows::update_model(workflow, nested_spec)))

  expect_true(is_nested(nested_spec))
  expect_false(is_nested(spec))
  expect_false(is_nested(1))
})

test_that("extract_inner_model works", {
  m <- parsnip::linear_reg() %>%
    nested()

  expect_equal(extract_inner_model(m), parsnip::linear_reg())

  wf <- workflows::workflow(spec = m)

  expect_equal(extract_inner_model(wf), parsnip::linear_reg())

  expect_error(extract_inner_model(1), class = "bad_class")
  expect_error(extract_inner_model(parsnip::linear_reg()))
})
