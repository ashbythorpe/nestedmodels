test_that("nest_data works", {
  id <- sample(c(rep(1, 5), rep(2, 5), rep(3, 5)))

  data <- tibble::tibble(
    id = id,
    x = 1:15
  )
  res <- nest_data(data, "x", "id")
  expect_equal(res$nested_data, tidyr::nest(data, data = -"id"))
  expect_equal(res$column, "data")
  expect_equal(
    tidyr::unnest(res$nested_data, "data")[res$order, ],
    data
  )

  data2 <- tibble::tibble(
    id = id,
    x = 1:15,
    y = sample(letters[1:15])
  )
  res2 <- nest_data(data2, c("x", "y"), "id")
  expect_equal(res2$nested_data, tidyr::nest(data2, data = -"id"))
  expect_equal(res2$column, "data")
  expect_equal(
    tidyr::unnest(res2$nested_data, "data")[res2$order, ],
    data2
  )


  data3 <- tibble::tibble(
    .nest_id = id,
    a = sample(1:15),
    x = 1:15,
    y = sample(letters[1:15])
  )
  res3 <- nest_data(data3, c("a", "x", "y"), ".nest_id")
  expect_equal(res3$nested_data, tidyr::nest(data3, data = -".nest_id"))
  expect_equal(res3$column, "data")
  expect_equal(
    tidyr::unnest(res3$nested_data, "data")[res3$order, ],
    data3
  )

  data4 <- tidyr::nest(data2, data = -"id")
  res4 <- nest_data(data4, c("x", "y"), "id")
  expect_equal(res4$nested_data, data4)
  expect_equal(res4$column, "data")
  expect_equal(
    tidyr::unnest(res4$nested_data, "data")[res4$order, ],
    tidyr::unnest(data4, "data")
  )

  data5 <- dplyr::group_by(data2, .data$id)
  res5 <- nest_data(data5, c("x", "y"), "id")
  expect_equal(res5$nested_data, dplyr::ungroup(
    tidyr::nest(data5, data = -"id")
  ))
  expect_equal(res5$column, "data")
  expect_equal(
    tidyr::unnest(res5$nested_data, "data")[res5$order, ],
    dplyr::ungroup(data5)
  )

  data6 <- dplyr::group_by(data2, .data$y, .data$id)
  res6 <- nest_data(data6, c("x", "y"), "id")
  expect_equal(res6$nested_data, tidyr::nest(dplyr::ungroup(data6),
    data = -"id"
  ))
  expect_equal(res6$column, "data")
  expect_equal(
    tidyr::unnest(res6$nested_data, "data")[res6$order, ],
    dplyr::ungroup(data6)
  )

  data7 <- dplyr::group_by(data2, .data$x, .data$y)
  res7 <- nest_data(data7, c("x", "y"), "id")
  expect_equal(res7$nested_data, tidyr::nest(dplyr::ungroup(data7),
    data = -"id"
  ))
  expect_equal(res7$column, "data")
  expect_equal(
    tidyr::unnest(res7$nested_data, "data")[res7$order, ],
    dplyr::ungroup(data7)
  )
})

test_that("find_nested_column_with_names works", {
  data <- tibble::tibble(
    id = c(rep(1, 5), rep(2, 5)),
    x = 1:10
  ) %>%
    tidyr::nest(data = -id)

  quiet_fun <- purrr::quietly(find_nested_column_with_names)

  expect_equal(find_nested_column_with_names(data, "x"), "data")
  expect_equal(find_nested_column_with_names(data, "a"), "data")
  expect_equal(find_nested_column_with_names(data, "id"), "data")

  data2 <- tibble::tibble(
    id = 1,
    x = list(tibble::tibble()),
    y = list(tibble::tibble())
  )
  expect_warning(find_nested_column_with_names(data2, "x"))
  expect_equal(
    quiet_fun(data2, "x")$result,
    "x"
  )
  data2$y <- list(tibble::tibble(x = 1))
  expect_equal(find_nested_column_with_names(data2, "x"), "y")
  expect_warning(find_nested_column_with_names(data2, c("x", "y")))
  expect_equal(
    quiet_fun(data2, c("x", "y"))$result,
    "y"
  )
  data2$x <- list(tibble::tibble(x = 1, y = 1))
  expect_equal(find_nested_column_with_names(data2, "y"), "x")
  expect_equal(find_nested_column_with_names(data2, c("x", "y")), "x")
  expect_warning(find_nested_column_with_names(data2, "x"))
  expect_equal(quiet_fun(data2, "x")$result, "x")

  data3 <- tibble::tibble(
    id = 1,
    x = list(tibble::tibble(x = 1)),
    y = list(tibble::tibble(
      x = 1, y = 2
    )),
    z = list(tibble::tibble(
      x = 1, y = 1
    )),
    a = list(tibble::tibble())
  )
  expect_warning(find_nested_column_with_names(data3, c("x", "y", "z")))
  expect_equal(quiet_fun(data3, c("x", "y", "z"))$result, "y")
  data3$y <- NULL
  expect_warning(find_nested_column_with_names(data3, c("x", "y", "z")))
  expect_equal(quiet_fun(data3, c("x", "y", "z"))$result, "z")

  data4 <- tibble::tibble()

  expect_error(find_nested_column_with_names(data4, "x"))

  data5 <- tibble::tibble(x = list())

  expect_error(find_nested_column_with_names(data5, "x"))
})
