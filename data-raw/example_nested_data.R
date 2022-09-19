## code to prepare `example_nested_data` dataset goes here

weights <- rnorm(4, 0, 1)

example_nested_data <- tibble::tibble(
  id = rep(1L:50L, each = 50),
  id2 = rep(1L:75L, each = 33, length.out = 2500),
  x = purrr::map(sample.int(1000L, 50L), ~ seq(., . + 49L)) %>%
    purrr::flatten_int(),
  y = 1L:2500L,
  z = weights[1]*rep(sample(1L:50L), each = 50) + 
    weights[2]*rep(sample(1L:75L), each = 33, length.out = 2500) + 
    weights[3]*y + weights[4]*x/100,
  a = runif(2500, 0, 100),
  b = rnorm(2500, 50, 25)
)

usethis::use_data(example_nested_data, overwrite = TRUE)
