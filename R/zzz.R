# nocov start
.onLoad <- function(libname, pkgname) {
  make_nested_model()

  safe_multi_predict <<- purrr::possibly(multi_predict,
    otherwise = NULL,
    quiet = FALSE
  )
  safe_predict <<- purrr::possibly(predict, otherwise = NULL, quiet = FALSE)

  safe_augment <<- purrr::possibly(augment, otherwise = NULL, quiet = FALSE)
}
# nocov end
