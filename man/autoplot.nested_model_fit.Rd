% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.nested_model_fit}
\alias{autoplot.nested_model_fit}
\title{Create a set of ggplots for a nested model object}
\usage{
\method{autoplot}{nested_model_fit}(object, ...)
}
\arguments{
\item{object}{A \code{nested_model_fit} object produced by
\code{\link[=fit.nested_model]{fit.nested_model()}}.}

\item{...}{Passed into \code{\link[parsnip:autoplot.model_fit]{parsnip::autoplot.model_fit()}}.}
}
\value{
A list of \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} objects.
}
\description{
This method calls \code{\link[parsnip:autoplot.model_fit]{parsnip::autoplot.model_fit()}} on each model fitted on
each nested data frame, returning a list of plots.
}
\details{
Printing the list of plots will print every plot in turn, so remember to
store the result of this function in a variable to look at each plot
individually.
}
\examples{
\dontshow{if (rlang::is_installed(c("patchwork", "glmnet", "ggrepel"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

library(dplyr)
library(tidyr)
library(purrr)
library(parsnip)
library(glmnet)
library(ggplot2)

data <- filter(example_nested_data, id \%in\% 16:20)

nested_data <- nest(data, data = -id2)

model <- linear_reg(penalty = 1) \%>\%
  set_engine("glmnet") \%>\%
  nested()

fit <- fit(model, z ~ x + y + a + b, nested_data)

plots <- autoplot(fit)

# View the first plot
plots[[1]]

# Use the patchwork package (or others) to combine the plots
library(patchwork)

reduce(plots, `+`)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[ggplot2:autoplot]{ggplot2::autoplot()}}
}
