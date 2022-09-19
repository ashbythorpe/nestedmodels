test_that("general tests", {
  fit_resamples(final_workflow, resamples) %>%
    expect_s3_class("resample_results")
})
