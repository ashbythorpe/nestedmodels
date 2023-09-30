# nestedmodels (development version)

# nestedmodels 1.1.0

* Added support for parallel processing in `nested()`.

* Used ".nest_id" instead of "nest_id" as the column name in `step_nest()` to reduce the likelihood of overwriting an existing column.

* Added an `autoplot()` method for the nested_model_fit object.

# nestedmodels 1.0.4

* Fixed the `prep.step_nest()` method to include an ellipsis argument, to ensure consistency with the generic.

# nestedmodels 1.0.3

* Used `expect_snapshot()` to test printing (Thanks to a contribution from 
Emil Hvitfeldt).

* Removed forcats dependency.

# nestedmodels 1.0.2

* Made sure the package works with the latest/development versions of tidyselect and purrr (Thanks to a contribution from Hadley Wickham).

# nestedmodels 1.0.1

* Removed all uses of `<<-` for CRAN resubmission.

# nestedmodels 1.0.0

* Submitted to CRAN.

* Added a `NEWS.md` file to track changes to the package.
