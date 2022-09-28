test_that("augment.nested_model_fit works", {
  model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    nested()
  
  nested_data <- tidyr::nest(example_nested_data, data = -id)
  
  fit <- fit(model, z ~ ., nested_data)
  
  suppressWarnings({
    expect_equal(augment(fit, example_nested_data),
                 augment(fit, nested_data))
    expect_equal(
      nrow(augment(fit, example_nested_data %>%
                     dplyr::filter(.data$id %in% c(9,10,11)))), 150
    )
  })
})
