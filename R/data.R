#' Example nested data
#'
#' A dataset containing example data that can be nested. Mainly used for
#' examples and testing.
#'
#' @format
#' A tibble with 1000 rows and 7 variables
#' \describe{
#'    \item{id}{A column that can be nested, ranging from 1 to 20.}
#'    \item{id2}{Another column that can be nested, ranging from 1 to 10.}
#'    \item{x}{A numeric column that depends on 'id'.}
#'    \item{y}{A sequential numeric column (with some added randomness),
#'    independent of the other columns.}
#'    \item{z}{A column dependent on id, id2, x and y.}
#'    \item{a}{A randomly generated numeric column, ranging from 1 to 100.}
#'    \item{b}{A randomly generated numeric column, centred around 50.}
#' }
#'
#' @examples
#' example_nested_data
"example_nested_data"
