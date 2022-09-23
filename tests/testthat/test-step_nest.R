test_that("step_nest works", {
  recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
    step_nest(.data$id)
  
  baked_data <- recipe %>%
    recipes::prep() %>%
    recipes::bake(NULL)
  
  data <- example_nested_data %>%
    tidyr::nest(data = -.data$id) %>%
    dplyr::select(data) %>%
    dplyr::mutate(nest_id = factor(glue::glue("Nest {dplyr::row_number()}"))) %>%
    tidyr::unnest(data) %>%
    dplyr::relocate(tidyselect::all_of(colnames(baked_data)))
  
  expect_equal(baked_data, data)
  
  nested_data2 <- dplyr::filter(example_nested_data, .data$id == 10)
  
  baked_data2 <- recipe %>%
    recipes::prep(example_nested_data) %>%
    recipes::bake(nested_data2) %>%
    dplyr::mutate(nest_id = as.character(.data$nest_id))
  
  data2 <- nested_data2 %>%
    dplyr::select(-.data$id) %>%
    dplyr::mutate(nest_id = "Nest 10") %>%
    dplyr::relocate(tidyselect::all_of(colnames(baked_data2)))
  
  expect_equal(baked_data2, data2)
})
