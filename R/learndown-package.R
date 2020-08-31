#' @details
#' The SciViews learndown package provides additional R Markdown tags to build
#' richer learning material with interactivity.
#'
#' See [h5p()] or [launch_shiny()]
#' @keywords internal
"_PACKAGE"

#' @importFrom glue glue
#' @importFrom webshot webshot
#' @importFrom magick image_read image_write image_composite image_info image_crop
#' @importFrom shinytoastr useToastr toastr_error toastr_success
#' @importFrom shinylogs store_rds track_usage
#' @importFrom shiny actionButton conditionalPanel fluidRow h4 isolate observe observeEvent onSessionEnded parseQueryString req tagList tags textInput updateTextInput
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
