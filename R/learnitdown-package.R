#' @details
#' The SciViews learnitdown package provides additional R Markdown tags to build
#' richer learning material with interactivity and to manage H5P exercises,
#' learnr tutorials and Shiny interactive applications with user identification
#' and activity tracking in a MongoDB database.
#'
#' See [h5p()] or [launch_shiny()]
#' @keywords internal
"_PACKAGE"

#' @importFrom learnr run_tutorial tutorial_options answer
#' @importFrom remotes install_github
#' @importFrom glue glue
#' @importFrom webshot webshot
#' @importFrom magick image_read image_write image_composite image_info image_crop
#' @importFrom shinytoastr useToastr toastr_error toastr_info toastr_success toastr_warning
#' @importFrom shinylogs read_rds_logs store_rds track_usage
#' @importFrom shiny actionButton conditionalPanel div fluidRow h1 h4 img isolate observe observeEvent onSessionEnded parseQueryString renderText req strong tagList stopApp tags textInput textOutput updateActionButton updateTextInput
#' @importFrom mongolite mongo
#' @importFrom httr authenticate content GET parse_url status_code
#' @importFrom utils askYesNo compareVersion create.post packageVersion select.list URLdecode URLencode read.csv write.csv
#' @importFrom jsonlite base64_dec base64_enc fromJSON toJSON
#' @importFrom PKI PKI.digest PKI.decrypt PKI.encrypt
#' @importFrom getPass getPass
#' @importFrom stats runif
#importFrom keyring key_set_with_value key_get
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
