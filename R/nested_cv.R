#' Combine multiple nested_cv objects
#'
#' Combine the results of multiple [rsample::nested_cv()] calls into one
#' `nested_cv` object.
#'
#' @param resamples A list of `nested_cv` objects.
#' @param data The unnested data frame.
#' @param x A list of data frames which each item of `data` was created using.
#' @param action A 'size_action' option (a string), used to match the sizes
#'   of inner and outer resamples.
#'
#' @returns A `nested_cv` object.
#'
#' @noRd
combine_nested_cv <- function(resamples, data, x, action) {
  res <- match_sizes(resamples, "truncate")
  format <- res$rset[[res$index]]
  splits <- purrr::map(res$rset, "splits") %>%
    purrr::pmap(~ {
      combine_rsets(list(...),
        x = x, data = data,
        format_index = res$index
      )
    })

  dfs <- purrr::map(splits, rsample::training)
  inner_res <- purrr::map(res$rset, "inner_resamples") %>%
    transpose_version()

  inner <- purrr::map(inner_res, match_sizes, action)
  formats <- purrr::map(inner, ~ {
    .$rset[[.$index]]
  })

  args <- list(
    splits = purrr::map(inner, "rset"),
    format = formats,
    index = purrr::map(inner, "index"),
    data = dfs
  )

  final_inner <- purrr::pmap(args, combine_nested_cvs)
  new_nested_rset(splits, format = format, inner_resamples = final_inner)
}

#' @noRd
combine_nested_cvs <- function(splits, format, index, data) {
  x <- purrr::map(splits, list("splits", 1, "data"))
  purrr::map(splits, "splits") %>%
    purrr::pmap(combine_rsets_dots,
      x = x, data = data,
      format_index = index
    ) %>%
    new_nested_rset(format = format)
}

combine_rsets_dots <- function(..., x, data, format_index) {
  combine_rsets(list(...), x = x, data = data, format_index = format_index)
}
