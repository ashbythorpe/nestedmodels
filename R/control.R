# control_nested <- function(verbose = F,
#                            allow_par = T,
#                            control = NULL) {
#   test_logical_arg(verbose)
#   test_logical_arg(allow_par)
#   if (!is.null(control) && 
#       !any(stringr::str_detect(class(control), "^control"))) {
#     cli::cli_abort(c(
#       "{.arg control} must be a control object",
#       "i" = "It must have a class that starts with {.val control}",
#       "i" = "Actual class: {.cls {class(control)}}"
#     ))
#   }
# 
#   list(verbose = verbose, allow_par = allow_par, control = control) %>%
#     new_control_nested()
# }
# 
# print.control_nested <- function(x, ...) {
#   cat("nested control object\n")
#   invisible(x)
# }
