#' Insert a block for a learnr tutorial
#'
#' Insert Markdown text to link to a learnr tutorial.
#'
#' @param id Identifier of the learnr tutorial (as `tutorial:id` in the YAML
#' section).
#' @param title The title of the tutorial (`title` in the YAML section). If
#' `NULL`, the `id` is used instead.
#' @param package Package where the learnr tutorial is defined.
#' @param toc Entry to use in the exercises table of content (`NULL` if no
#' entry, `""` for a default entry based on `toc.def =`).
#' @param text The text to display in the learnr block.
#' @param toc.def Text for a default toc entry using [glue()] syntax for
#' replacement, e.g., `{id}`.
#' @param rstudio.url The URL to open a page in RStudio server in the SciViews
#' box.
#' @param tuto.img The image to display in front of the toc entry
#' @param tuto.link The link when the image is clicked (sends to an help page
#' about learnr tutorials).
#'
#' @return The Markdown chunk to insert a learnr tutorial block in the document.
#' @export
learnr <- function(id, title = NULL, package, toc = "",
text = "Now let's make the exercises in the following tutorial:",
toc.def = "Tutorial {id}", rstudio.url = "start_rstudio.html",
tuto.img = "images/list-tuto.png", tuto.link = "tutorial") {
  if (!is.null(toc)) {
    # Add an entry in the ex_toc
    ex_toc <- getOption("learndown_ex_toc", "")
    if (toc == "") {
      # Use default text
      toc <- glue::glue(toc.def)
    }
    ex_toc <- paste0(ex_toc, "\n",
      glue::glue("- [![tuto]({tuto.img})]({tuto.link}) [{toc}](#{id})"))
    options(learndown_ex_toc = ex_toc)
  }

  if (is.null(title)) {
    title <- id
  } else {
    title <- paste0(id, " (", title, ")")
  }

  url <- paste0(rstudio.url, "?runrcode=", package, "%3A%3Arun%28%22",
    URLencode(id, reserved = TRUE), "%22%29")

  glue::glue("\n\n\\BeginKnitrBlock{{tuto}}<div class=\"tuto\">
{text} **[]{{#{id} }[{title}]({url}){{target=\"_blank\"}}**.

    {package}::run(\"{id}\")

</div>\\EndKnitrBlock{{tuto}}\n\n")
}
