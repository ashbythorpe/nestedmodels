% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested.R
\name{extract_inner_model}
\alias{extract_inner_model}
\alias{extract_inner_model.default}
\alias{extract_inner_model.nested_model}
\alias{extract_inner_model.workflow}
\alias{extract_inner_model.model_spec}
\title{Get the inner model of a nested model object}
\usage{
extract_inner_model(x, ...)

\method{extract_inner_model}{default}(x, ...)

\method{extract_inner_model}{nested_model}(x, ...)

\method{extract_inner_model}{workflow}(x, ...)

\method{extract_inner_model}{model_spec}(x, ...)
}
\arguments{
\item{x}{A model spec or workflow.}

\item{...}{Not used.}
}
\value{
A \code{model_spec} object
}
\description{
Extract the inner model of a \code{nested_model} object, or a workflow
containing a nested model.
}
\examples{

library(parsnip)

model <- linear_reg() \%>\%
  set_engine("lm") \%>\%
  nested()

extract_inner_model(model)

}
