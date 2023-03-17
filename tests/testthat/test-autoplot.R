test_that("autoplot.nested_model_fit() works", {
  skip_if_not_installed("withr")
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("ggrepel")
  skip_if_not_installed("patchwork")
  
  withr::local_seed(42)
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)

  model <- parsnip::linear_reg(penalty = 1) %>%
    parsnip::set_engine("glmnet") %>%
    nested()

  fit <- fit(model, z ~ x + y + a + b, nested_data)

  plots <- ggplot2::autoplot(fit)

  vdiffr::expect_doppelganger("First autoplot result", plots[[1]])

  withr::with_package("patchwork", {
    vdiffr::expect_doppelganger(
      "Combined autoplot results",
      purrr::reduce(plots, `+`)
    )
  })
})
