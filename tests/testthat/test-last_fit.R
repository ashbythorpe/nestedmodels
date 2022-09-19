test_that("general tests", {
  last_fit(final_workflow, initial_split) %>%
    expect_s3_class("last_fit")
})
