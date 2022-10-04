# nocov start
.onLoad <- function(libname, pkgname) {
  make_nested_model()

  safe_multi_predict <<- purrr::possibly(parsnip::multi_predict,
    otherwise = NULL,
    quiet = FALSE
  )

  safe_predict <<- purrr::possibly(stats::predict,
    otherwise = NULL,
    quiet = FALSE
  )

  safe_augment <<- purrr::possibly(generics::augment,
    otherwise = NULL,
    quiet = FALSE
  )
}
# nocov end
