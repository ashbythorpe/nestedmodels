library(recipes)

recipe <- recipe(example_nested_data, ~.) |>
  step_group_by(id) |>
  step_mutate(x = as.factor(x)) |>
  step_dummy(x) |>
  step_ungroup()

data <- recipe |>
  prep() |>
  bake(NULL)

data$x_X50

recipe$steps[[1]]$inner_steps

example_nested_data
