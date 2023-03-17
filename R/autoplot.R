#' Create a set of ggplots for a nested model object
#' 
#' This method calls [parsnip::autoplot.model_fit()] on each model fitted on
#' each nested data frame, returning a list of plots.
#' 
#' @param object A `nested_model_fit` object produced by
#'   [fit.nested_model()].
#' @param ... Passed into [parsnip::autoplot.model_fit()].
#' 
#' @details 
#' Printing the list of plots will print every plot in turn, so remember to
#' store the result of this function in a variable to look at each plot
#' individually.
#' 
#' @returns A list of [ggplot2::ggplot()] objects.
#' 
#' @seealso [ggplot2::autoplot()]
#' 
#' @examplesIf rlang::is_installed(c("patchwork", "glmnet", "ggrepel"))
#' 
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' library(parsnip)
#' library(glmnet)
#' library(ggplot2)
#' 
#' data <- filter(example_nested_data, id %in% 16:20)
#'
#' nested_data <- nest(data, data = -id2)
#'
#' model <- linear_reg(penalty = 1) %>%
#'   set_engine("glmnet") %>%
#'   nested()
#'
#' fit <- fit(model, z ~ x + y + a + b, nested_data)
#' 
#' plots <- autoplot(fit)
#' 
#' # View the first plot
#' plots[[1]]
#' 
#' # Use the patchwork package (or others) to combine the plots
#' library(patchwork)
#' 
#' reduce(plots, `+`)
#' 
#' @importFrom ggplot2 autoplot
#' 
#' @export
autoplot.nested_model_fit <- function(object, ...) {
  purrr::map(object$fit$.model_fit, ggplot2::autoplot, ...)
}
