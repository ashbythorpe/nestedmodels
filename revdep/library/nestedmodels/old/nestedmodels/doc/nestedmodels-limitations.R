## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(nestedmodels)
library(parsnip)

## ----time---------------------------------------------------------------------
model <- linear_reg() %>%
  set_engine("lm") %>%
  nested()

system.time({
  fit(model, z ~ ., tidyr::nest(example_nested_data, data = -id))
})

## ----size---------------------------------------------------------------------
utils::object.size(fit)

