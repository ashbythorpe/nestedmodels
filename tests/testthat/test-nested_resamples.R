test_that("nested_resamples works", {
  skip_if_not_installed("withr")
  skip_if_not_installed("workflows")

  withr::local_seed(42)

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

  sample_data <- data.frame(x = 1:10)

  nested_data <- tidyr::nest(example_nested_data, data = -id)

  expect_error(nested_resamples(example_nested_data, rsample::vfold_cv()))

  nested_resamples(nested_data, rsample::vfold_cv()) %>%
    expect_s3_class(class(rsample::vfold_cv(sample_data, 2)))

  nested_resamples(
    dplyr::group_by(example_nested_data, id),
    ~ rsample::initial_split(.)
  ) %>%
    expect_s3_class(class(rsample::initial_split(sample_data)))

  nested_resamples(example_nested_data, rsample::bootstraps,
    times = 25,
    nesting_method = wf
  ) %>%
    expect_s3_class(class(rsample::bootstraps(sample_data, 1)))

  nested_resamples(dplyr::group_by(example_nested_data, id), diff_lens,
    size_action = "recycle-random"
  ) %>%
    expect_s3_class(class(rsample::vfold_cv(sample_data, 2)))

  nested_resamples(dplyr::group_by(example_nested_data, id), "diff_lens",
    size_action = "combine-random"
  ) %>%
    expect_s3_class(class(rsample::vfold_cv(sample_data, 2)))

  small_data <- tibble::tibble(
    id = c(rep(1, 10), rep(2, 10), rep(3, 10)),
    x = 1:30
  )

  nested_resamples(
    tidyr::nest(small_data, data = -"id"),
    rsample::nested_cv(
      rsample::vfold_cv(),
      rsample::bootstraps(5)
    )
  ) %>%
    expect_s3_class(class(rsample::nested_cv(
      sample_data,
      rsample::vfold_cv(2),
      rsample::bootstraps(1)
    )))
})
