## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(nestedmodels)
library(tidyr)
library(parsnip)
library(recipes)
library(workflows)
library(rsample)
library(glmnet)

## ----data---------------------------------------------------------------------
data("example_nested_data")
data <- example_nested_data
data

## ----nested-data--------------------------------------------------------------
nested_data <- nest(data, data = -id)
nested_data

## -----------------------------------------------------------------------------
split <- nested_resamples(nested_data, rsample::initial_split())
data_tr <- rsample::training(split)
data_tst <- rsample::testing(split)

## ----create-model-------------------------------------------------------------
model <- linear_reg(penalty = 0.1) %>%
  set_engine("glmnet")

## ----nested-model-------------------------------------------------------------
nested_model <- model %>%
  nested()
nested_model

## ----fit-model----------------------------------------------------------------
nested_tr <- tidyr::nest(data_tr, data = -id)
model_fit <- fit(nested_model, z ~ x + y + a + b, nested_tr)
model_fit

## ----predict-model, R.options = list(warnPartialMatchArgs = FALSE)------------
predict(model_fit, data_tst)

## ----recipe-------------------------------------------------------------------
recipe <- recipe(data_tr, z ~ x + y + a + b + id) %>%
  step_nest(id)

## ----bake---------------------------------------------------------------------
recipe %>%
  prep() %>%
  bake(NULL)

## ----workflow-----------------------------------------------------------------
wf <- workflow() %>%
  add_model(nested_model) %>%
  add_recipe(recipe)

## ----workflow-fit-------------------------------------------------------------
wf_fit <- fit(wf, data_tr)

## ----workflow-predict, R.options = list(warnPartialMatchArgs = FALSE)---------
predict(wf_fit, data_tst)

## ----tidy, R.options = list(warnPartialMatchArgs = FALSE)---------------------
augment(wf_fit, data_tst)
tidy(wf_fit)

