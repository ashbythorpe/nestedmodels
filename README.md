
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nestedmodels

<!-- badges: start -->

[![R-CMD-check](https://github.com/ashbythorpe/nestedmodels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashbythorpe/nestedmodels/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/nestedmodels)](https://CRAN.R-project.org/package=nestedmodels)
[![Codecov test
coverage](https://codecov.io/gh/ashbythorpe/nestedmodels/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ashbythorpe/nestedmodels?branch=master)
<!-- badges: end -->

The goal of nestedmodels is to allow the modelling of nested data. Some
models only accept certain predictors. For panel data, it is often
desirable to create a model for each panel. nestedmodels enhances the
‘[tidymodels](https://www.tidymodels.org/)’ set of packages by allowing
the user to classify a model as ‘nested’.

## Installation

You can install the development version of nestedmodels like so:

``` r
# install.packages("devtools")
devtools::install_github("ashbythorpe/nestedmodels")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(nestedmodels)

data("example_nested_data")

nested_data <- tidyr::nest(example_nested_data, data = -id)

split <- nested_resamples(nested_data, rsample::initial_split())

data_tr <- rsample::training(split)
data_tst <- rsample::testing(split)

model <- parsnip::linear_reg() %>%
  nested()

fit <- fit(model, z ~ x + y + a + b, 
           tidyr::nest(data_tr, data = -id))

predict(fit, data_tst)
#> # A tibble: 260 × 1
#>    .pred
#>    <dbl>
#>  1 38.9 
#>  2 11.5 
#>  3 34.0 
#>  4 33.2 
#>  5 25.3 
#>  6 26.2 
#>  7 42.5 
#>  8  8.57
#>  9 34.7 
#> 10 49.0 
#> # … with 250 more rows
```
