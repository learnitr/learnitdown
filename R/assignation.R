#' Insert a GitHub assignation in the document
#'
#' Insert a block of class `assign` with data related to a GitHub (Classroom)
#' assignment.
#'
#' @param name The name of the assignation (usually the same as the name of the
#' template GitHub repository).
#' @param url The URL of the assignation (could be a named list for different
#' versions depending on the course).
#' @param course.urls Named vector with the Classroom URLS for each course.
#' Names are the course identifiers in Moodle. If `NULL`, no course-specific
#' sections are added.
#' @param part If the assignation presents several parts in the `README.md` file
#' (because the same assignation serves at different places), indicate the part
#' here. Otherwise, leave the default value `NULL` if there are no parts.
#' @param course.names A named character vector with the name of the course.
#' Names used must be the same as for `course.urls`, but there may be more here,
#' and not necessarily in the same order.
#' @param toc The exercise table of content (ex-toc) label. If `toc = ""`
#' (default), a generic label is calculated using `toc.def` from `texts`.
#' @param texts Various sentences used to construct the assignation bloc. You
#' can make a call to `assignation_en()` or `assignation_fr()` as a basis
#' and modify only the sentence you want.
#' @param assign.img The relative path to the image to use before the label in
#' the ex-toc.
#' @param assign.link The link to the learnr help page (when the user clicks on
#' the image in the ex-toc).
#' @param title The title of the block.
#' @param part.name The word to use for "part".
#' @param alt The text to display for alternate access to the repository (for
#' non-registered users). Use a string following the [glue()] syntax to replace
#' variables.
#' @param sub The text that appears at the bottom of the assignation block.
#' @param course The text for items corresponding to courses.
#' @param toc.def The default ex-toc label (using [glue()] syntax).
#'
#' @return Markdown code that generates the GitHub assignation block. It is most
#' conveniently used inside and R inline expression in your R Markdown document
#' on its own line with one blank line above and bellow it.
#' @export
#'
#' @details
#' If the URL contains several entries, names are used to create show/hide divs
#' according to the `icourse` user information.
assignation <- function(name, url, course.urls = NULL, part = NULL,
course.names = c(course1 = "Data Science"), toc = "",
texts = assignation_en(), assign.img = "images/list-assign.png",
assign.link = "github_assignation") {
  if (is.null(part)) {
    anchor <- name
    part <- ""
  } else {
    # Anchor is a combination of name and part to separate different parts
    anchor <- paste0(name, part)
    part <- paste(",", texts$part.name, part)
  }

  if (!is.null(toc)) {
    # Add an entry in the ex_toc
    ex_toc <- getOption("learndown_ex_toc", "")
    if (toc == "") {
      # Use default text
      toc <- glue::glue(texts$toc.def)
    }
    ex_toc <- paste0(ex_toc, "\n",
      glue::glue("- [![git]({assign.img})]({assign.link}) [{toc}](#{anchor})"))
    options(learndown_ex_toc = ex_toc)
  }

  if (is.null(course.urls) || !length(course.urls)) {
    course_text <- ""
  } else {# for each course, create one entry
    course_ids <- names(course.urls)
    course_names <- course.names[course_ids]
    course_text <- ""
    for (i in 1:length(course_ids)) {
      course_id <- course_ids[i]
      course_url <- course.urls[i]
      course_name <- course_names[i]
      course_text <- paste0(course_text, "\n", glue::glue("\n::: {{ .{course_id} }}
**[{texts$course} {course_name}]({course_url})**
\n:::\n\n"))
    }
  }

  alt_text <- glue::glue(texts$alt)

  glue::glue("\n\\BeginKnitrBlock{{assign}}<div class=\"assign\">
{texts$title} **[{name}]{{#{anchor} }}{part}**.

{course_text}

{alt_text}

*{texts$sub}{part}.*
</div>\\EndKnitrBlock{{assign}}\n\n")
}

#' @rdname assignation
#' @export
assignation_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Complete this assignation:",
    part.name = "part",
    alt       = "If you are not a registered user, or if you work outside of a course, fork [this]({url}) repository.",
    sub      = "See the explanations in the `README.md`",
    course   = "Assignation for the students enrolled at the course",
    toc.def  = "Assignation {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignation
#' @export
assignation_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "R\u00e9alisez l'assignation",
    part.name = "partie",
    alt       = "Si vous \u00eates un utilisateur non enregistr\u00e9 ou que vous travaillez en dehors d'un cours, faites un \"fork\" de [ce]({url}) d\u00e9p\u00f4t.",
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = "Assignation pour les \u00e9tudiants inscrits au cours de",
    toc.def   = "Assignation {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}
