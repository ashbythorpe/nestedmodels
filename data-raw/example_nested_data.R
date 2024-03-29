## code to prepare `example_nested_data` dataset goes here
library(magrittr)

set.seed(42)

weights <- rnorm(4, 0, 1)

example_nested_data <- tibble::tibble(
  id = rep(1L:20L, each = 50),
  id2 = c(rep(1L, 25L), rep(2L:10L, each = 100), rep(1L, 75L)),
  x = purrr::map(sample.int(1000L, 20L), ~ seq(., . + 49L)) %>%
    purrr::flatten_int(),
  y = 1L:1000L + runif(1000, -100, 100),
  z = weights[1] * rep(sample(1L:50L), each = 20) +
    weights[2] * rep(sample(1L:75L), each = 13, length.out = 1000) +
    weights[3] * y / 10 + weights[4] * x / 100,
  a = runif(1000, 0, 100),
  b = rnorm(1000, 50, 25)
)

usethis::use_data(example_nested_data, overwrite = TRUE)
