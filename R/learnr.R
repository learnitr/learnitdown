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
#' @param connect.url The URL to open a page in Posit Connect. If provided, it
#' is used instead of `rstudio.url`.
#' @param tuto.img The image to display in front of the toc entry
#' @param tuto.link The link when the image is clicked (sends to an help page
#' about learnr tutorials).
#' @param icourse The course identifier, e.g., `"MATH101"`.
#' @param institution The institution name, e.g., `"My University"`.
#' @param acad_year The academic year, e.g., `"2023-2024"`.
#' @param term The term, e.g., `"Q1"`.
#' @param set The set identifier, e.g., `"21M"` where 21 is the year and M is a
#' set identifier.
#'
#' @return The Markdown chunk to insert a learnr tutorial block in the document.
#' @export
learnr <- function(id, title = NULL, package, toc = "",
text = "Now let's make the exercises in the following tutorial:",
toc.def = "Tutorial {id}", rstudio.url = "start_rstudio.html",
connect.url = NULL,
tuto.img = "images/list-tuto.png", tuto.link = "tutorial",
icourse = "", institution = "", acad_year = "", term = "", set = "") {
  if (is.null(title)) {
    title <- id
  } else {
    title <- paste0(id, " (", title, ")")
  }

  if (!is.null(connect.url)) {
    if (!endsWith(connect.url, "/"))
      connect.url <- paste0(connect.url, "/")
    url <- paste0(connect.url, URLencode(id, reserved = TRUE), "/")
  } else {# Use RStudio URL instead
    url <- paste0(rstudio.url, "?runrcode=", package, "%3A%3Arun%28%22",
      URLencode(id, reserved = TRUE), "%22%29")
  }

  if (!is.null(toc)) {
    # Add an entry in the ex_toc
    ex_toc <- getOption("learnitdown_ex_toc", "")
    if (toc == "") {
      # Use default text
      toc <- glue::glue(toc.def)
    }
    ex_toc <- paste0(ex_toc, "\n",
      glue::glue("- [![tuto]({tuto.img})]({tuto.link}) [{toc}](#{id})"))
    options(learnitdown_ex_toc = ex_toc)

    # Also add an entry in the apps
    apps <- getOption("learnitdown_apps", data.frame())
    app <- data.frame(
      app         = id,
      type        = "learnr",
      icourse     = icourse,
      institution = institution,
      course      = substring(id, 1, 1),
      acad_year   = acad_year,
      term        = term,
      module      = substring(id, 1, 3),
      set         = set,
      assignment  = NA_character_,
      template    = NA_character_,
      url         = connect.url,
      alt_url     = url,
      start       = NA,
      end         = NA,
      deadline    = NA,
      part        = NA,
      toc         = TRUE,
      clone       = FALSE,
      n           = 1,
      level       = 2,
      weight      = 1) # Always 1 for now
    apps <- rbind(apps, app)
    options(learnitdown_apps = apps)
  }

  glue::glue("\n\n\\BeginKnitrBlock{{tuto}}<div class=\"tuto\">
{text} **[]{{#{id} }[{title}]({url}){{target=\"_blank\"}}**.

    {package}::run(\"{id}\")

</div>\\EndKnitrBlock{{tuto}}\n\n")
}
