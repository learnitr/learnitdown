#' @details
#' The SciViews learndown package provides additional R Markdown tags to build
#' richer learning material with interactivity.
#'
#' See [h5p()] or [launch_shiny()]
#' @keywords internal
"_PACKAGE"

#' @importFrom learnr run_tutorial
#' @importFrom remotes install_github
#' @importFrom glue glue
#' @importFrom webshot webshot
#' @importFrom magick image_read image_write image_composite image_info image_crop
#' @importFrom shinytoastr useToastr toastr_error toastr_info toastr_success toastr_warning
#' @importFrom shinylogs read_rds_logs store_rds track_usage
#' @importFrom shiny actionButton conditionalPanel fluidRow h4 isolate observe observeEvent onSessionEnded parseQueryString req tagList stopApp tags textInput updateActionButton updateTextInput
#' @importFrom mongolite mongo
#' @importFrom httr authenticate content GET parse_url status_code
#' @importFrom utils compareVersion create.post packageVersion select.list
#' @importFrom jsonlite base64_dec base64_enc fromJSON toJSON
#' @importFrom PKI PKI.digest PKI.decrypt PKI.encrypt
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
