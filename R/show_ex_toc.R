#' Insert a table of content for the exercises
#'
#' For the various exercise types (h5p, shiny apps, learnrs & GitHub
#' assignations) we add toc entries with [h5p()], [launch_shiny()], [learnr()],
#' and [assignation()], respectively. This function create the exercises toc.
#'
#' @param header A Markdown text to place as header of the exercises toc.
#' @param clear.it Do we clear the toc list (`TRUE` by default)
#'
#' @return The Markdown chunk with the exercises toc.
#' @export
show_ex_toc <- function(header = "", clear.it = TRUE) {
  ex_toc <- getOption("learndown_ex_toc", NULL)
  if (is.null(ex_toc)) {
    toc <- ""
  } else {
    toc <- paste0("\n", header, '\n', ex_toc, "\n\n")
    if (isTRUE(clear.it))
      options(learndown_ex_toc = NULL)
  }

  toc
}
