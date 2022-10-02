test_that("nested_resamples works", {
  expect_error(nested_resamples(NULL), class = "bad_type")

  expect_error(nested_resamples(tibble::tibble(x = 1), rsample::vfold_cv(),
    nesting_method = 1
  ), class = "bad_type")

  expect_error(nested_resamples(example_nested_data))

  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(id)

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe)

  diff_lens <- function(data) {
    if (sample(c(FALSE, TRUE), 1)) {
      rsample::vfold_cv(data, 5)
    } else {
      rsample::vfold_cv(data, 10)
    }
  }

  nested_data <- tidyr::nest(example_nested_data, data = -id)

  expect_error(nested_resamples(example_nested_data, rsample::vfold_cv()))

  nested_resamples(nested_data, rsample::vfold_cv()) %>%
    expect_s3_class(class(rsample::vfold_cv(example_nested_data)))
  nested_resamples(
    dplyr::group_by(example_nested_data, id),
    ~ rsample::initial_split(.)
  ) %>%
    expect_s3_class(class(rsample::initial_split(example_nested_data)))
  nested_resamples(example_nested_data, ~ {
    rsample::validation_split(.)
  },
  nesting_method = recipe
  ) %>%
    expect_s3_class(class(rsample::validation_split(example_nested_data)))
  nested_resamples(example_nested_data, rsample::bootstraps,
    times = 25,
    nesting_method = wf
  ) %>%
    expect_s3_class(class(rsample::bootstraps(example_nested_data)))

  nested_resamples(example_nested_data, "validation_split",
    nesting_method = recipe
  ) %>%
    expect_s3_class(class(rsample::validation_split(example_nested_data)))

  withr::with_seed(42, {
    nested_resamples(dplyr::group_by(example_nested_data, id), diff_lens,
      size_action = "recycle-random"
    ) %>%
      expect_s3_class(class(rsample::vfold_cv(example_nested_data)))
    nested_resamples(dplyr::group_by(example_nested_data, id), "diff_lens",
      size_action = "combine-random"
    ) %>%
      expect_s3_class(class(rsample::vfold_cv(example_nested_data)))
  })

  small_data <- tibble::tibble(
    id = c(rep(1, 10), rep(2, 10), rep(3, 10)),
    x = 1:30
  )
  nested_resamples(
    tidyr::nest(small_data, data = -.data$id),
    rsample::nested_cv(
      rsample::vfold_cv(),
      rsample::bootstraps()
    )
  ) %>%
    expect_s3_class(class(rsample::nested_cv(
      small_data,
      rsample::vfold_cv(),
      rsample::bootstraps()
    )))
})
