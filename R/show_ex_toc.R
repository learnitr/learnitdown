#' Insert a table of content for the exercises at the end of a bookdown chapter
#'
#' For the various exercise types (h5p, shiny apps, learnrs & GitHub
#' assignments/challenges) we add toc entries with [h5p()], [launch_shiny()],
#' [learnr()], [assignment()], and [challenge()] respectively. This function
#' creates the table of content for all these exercises.
#'
#' @param header A Markdown text to place as header of the exercises toc.
#' @param clear.it Do we clear the toc list (`TRUE` by default)?
#'
#' @return The Markdown chunk with the exercises toc.
#' @export
show_ex_toc <- function(header = "", clear.it = TRUE) {
  ex_toc <- getOption("learnitdown_ex_toc", NULL)
  if (is.null(ex_toc)) {
    toc <- ""
  } else {
    toc <- paste0("\n", header, '\n', ex_toc, "\n\n")
    if (isTRUE(clear.it))
      options(learnitdown_ex_toc = NULL)
  }

  # Also compile a single assignments.csv file
  ex_dir <- file.path(.get_output_dir(), "ex")
  files <- dir(ex_dir, pattern = "^assignment_.+\\.csv$", full.names = TRUE)
  res <- data.frame()
  for (file in files)
    res <- rbind(res, read.csv(file))
  write.csv(res, file.path(ex_dir, "assignments.csv"), row.names = FALSE)
  toc
}

#' @rdname show_ex_toc
#' @export
clean_ex_toc <- function() {
  ex_dir <- file.path(.get_output_dir(), "ex")
  unlink(ex_dir, recursive = TRUE)
  dir.create(ex_dir, showWarnings = FALSE, recursive = TRUE)
  invisible(NULL)
}

# Get the output_dir for a {bookdown} compilation
.get_output_dir <- function() {
  if (file.exists("_bookdown.yml")) {
    config <- readLines("_bookdown.yml")
    output_dir <- config[grepl("output_dir", config)]
    if (!length(output_dir)) {
      "docs" # Default value
    } else {
      sub("^.*output_dir.*['\"](.+)['\"].*$", "\\1", output_dir)
    }
  } else "docs" # Default value
}

# Format a POSIXct value so that it can be used in JavaScript
.format_js_time <- function(time) {
  sub("^(.+)(00)$", "\\1:\\2", format(as.POSIXct(time),
    format = "%Y-%m-%dT%H:%M:%OS3%z"))
}
