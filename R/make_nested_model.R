# nocov start
make_nested_model <- function() {
  current <- parsnip::get_model_env()
  if (!"nested_model" %in% current$models) {
    parsnip::set_new_model("nested_model")
  }

  parsnip::set_model_mode(model = "nested_model", mode = "classification")
  parsnip::set_model_mode(model = "nested_model", mode = "regression")
  parsnip::set_model_engine(
    model = "nested_model",
    mode = "classification",
    eng = "nestedmodels"
  )
  parsnip::set_model_engine(
    model = "nested_model",
    mode = "regression",
    eng = "nestedmodels"
  )
  parsnip::set_dependency(
    "nested_model",
    eng = "nestedmodels",
    pkg = "nestedmodels"
  )
}
# nocov end
