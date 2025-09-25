#' Insert H5P content in the document
#'
#' @param id The ID of the H5P content in your Wordpress.
#' @param name The name you give to your H5P content.
#' @param baseurl The first part of the URL for your domain, usually something
#' like `https://my.site.com` **without** the trailing `/`.
#' @param idurl The URL to the H5P content, usually something like `""`
#' for exercises from h5p.org or h5p.com, or
#' `"wp-admin/admin-ajax.php?action=h5p_embed&id="` for Wordpress
#' (by default).
#' @param width The width of the iframe where the H5P content is displayed.
#' @param height The height of the iframe.
#' @param toc Entry to use in the exercises table of content (`NULL` if no
#' entry, `""` for a default entry based on `toc.def =`).
#' @param toc.def Text for a default toc entry using [glue()] syntax for
#' replacement, e.g., `{var}`.
#' @param h5p.img The image to display in front of the toc entry
#' @param h5p.link The link when the image is clicked (sends to an help page
#' about learnr tutorials).
#' @param icourse The course identifier, e.g., `"MATH101"`.
#' @param institution The institution name, e.g., `"My University"`.
#' @param acad_year The academic year, e.g., `"2023-2024"`.
#' @param term The term, e.g., `"Q1"`.
#' @param set The set identifier, e.g., `"21M"` where 21 is the year and M is a
#' set identifier.
#'
#' @return HTML code that generates the iframe. It is most conveniently used
#' inside and R inline expression in your R Markdown document on its own line
#' with one blank line above and bellow it.
#' @export
#'
#' @details
#' This function is designed to work inside a Wordpress site where the H5P
#' plugin has been installed. You should also serve your bookdown pages as a
#' subdirectory inside of the same Wordpress site to allow free communication
#' between the parent (the bookdown page) and the child document in the iframe
#' (the H5P content).
h5p <- function(id, name, baseurl,
    idurl = "wp-admin/admin-ajax.php?action=h5p_embed&id=",
    width = 780, height = 500, toc = "", toc.def = "H5P exercise {id}",
    h5p.img = "images/list-h5p.png", h5p.link = "h5p", icourse = "",
    institution = "", acad_year = "", term = "", set = "") {
  if (!is.null(toc)) {
    # Add an entry in the ex_toc
    ex_toc <- getOption("learnitdown_ex_toc", "")
    if (toc == "") {
      # Use default text
      toc <- glue::glue(toc.def)
    }
    ex_toc <- paste0(ex_toc, "\n",
      glue::glue("- [![h5p]({h5p.img})]({h5p.link}) [{name} - {toc}](#h5p_{id})"))
    options(learnitdown_ex_toc = ex_toc)

    # Also add an entry in the apps
    apps <- getOption("learnitdown_apps", data.frame())
    app <- data.frame(
      app         = name,
      type        = "h5p",
      icourse     = icourse,
      institution = institution,
      course      = substring(name, 1, 1),
      acad_year   = acad_year,
      term        = term,
      module      = substring(name, 1, 3),
      set         = set,
      assignment  = NA_character_,
      template    = NA_character_,
      url         = paste0(baseurl, "/", idurl, id),
      alt_url     = paste0(baseurl, "/", idurl, id),
      start       = NA,
      end         = NA,
      deadline    = NA,
      part        = NA,
      toc         = TRUE,
      clone       = FALSE,
      n           = 1,
      level       = 1,
      weight      = 1) # Always 1 for now
    apps <- rbind(apps, app)
    options(learnitdown_apps = apps)
  }
  # In H5P wordpress plugin 1.15.2, there is title="' . $title . '" inside the iframe attributes that is added
  glue::glue("\n[]{{#h5p_{id}}}[![h5p]({h5p.img})]({h5p.link})\n<iframe src=\"{baseurl}/{idurl}{id}\" width=\"{width}\" height=\"{height}\" frameborder=\"0\" allowfullscreen=\"allowfullscreen\" class=\"h5p\"></iframe><script src=\"{baseurl}/wp-content/plugins/h5p/h5p-php-library/js/h5p-resizer.js\" charset=\"UTF-8\"></script>\n")
}
