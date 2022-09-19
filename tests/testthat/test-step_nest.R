recipe <- recipes::recipe(example_nested_data, z ~ .) %>%
  step_nest(id)

recipe %>%
  recipes::prep() %>%
  recipes::bake(NULL)

names <- list("id")
example_nested_data
dplyr::group_by(example_nested_data, !!!rlang::syms(names)) %>%
  tidyr::nest()
rlang::syms(names)
rlang::ensyms(names)
