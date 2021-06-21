#' Insert a GitHub assignation or challenge in the document
#'
#' Insert a block of class `assign`, `assign2`, `challenge` or `challenge2` with data related to a GitHub (Classroom)
#' assignment.
#'
#' @param name The name of the assignation or the challenge (usually the same as the name of the
#' template GitHub repository).
#' @param url The URL of the assignation or challenge (could be a named list for different
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
#' @param block The class of the div, or the LaTeX environment to use for the assignment block.
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
assign.link = "github_assignation", block = "assign") {
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
    ex_toc <- getOption("learnitdown_ex_toc", "")
    if (toc == "") {
      # Use default text
      toc <- glue::glue(texts$toc.def)
    }
    ex_toc <- paste0(ex_toc, "\n",
      glue::glue("- [![git]({assign.img})]({assign.link}) [{toc}](#{anchor})"))
    options(learnitdown_ex_toc = ex_toc)
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
      course_text <- paste0(course_text, "\n",
        glue::glue("\n::: {{ .{course_id} }}
**[{texts$course} {course_name}]({course_url}){{target=\"_blank\"}}**
\n:::\n\n"))
    }
  }

  alt_text <- glue::glue(texts$alt)

  glue::glue("\n\\BeginKnitrBlock{{{block}}}<div class=\"{block}\">
{texts$title} **[{name}]{{#{anchor} }}{part}**.

{course_text}

{alt_text}

*{texts$sub}{part}.*
</div>\\EndKnitrBlock{{{block}}}\n\n")
}

#' @rdname assignation
#' @export
assignation2 <- function(name, url, course.urls = NULL, part = NULL,
course.names = c(course1 = "Data Science"), toc = "",
texts = assignation2_en(), assign.img = "images/list-assign2.png",
assign.link = "github_assignation", block = "assign2")
  assignation(name, url, course.urls = course.urls, part = part,
    course.names = course.names, toc = toc, texts = texts,
    assign.img = assign.img, assign.link = assign.link, block = block)

#' @rdname assignation
#' @export
challenge <- function(name, url, course.urls = NULL, part = NULL,
  course.names = c(course1 = "Data Science"), toc = "",
  texts = challenge_en(), assign.img = "images/list-challenge.png",
  assign.link = "github_assignation", block = "challenge")
  assignation(name, url, course.urls = course.urls, part = part,
    course.names = course.names, toc = toc, texts = texts,
    assign.img = assign.img, assign.link = assign.link, block = block)

#' @rdname assignation
#' @export
challenge2 <- function(name, url, course.urls = NULL, part = NULL,
  course.names = c(course1 = "Data Science"), toc = "",
  texts = challenge2_en(), assign.img = "images/list-challenge2.png",
  assign.link = "github_assignation", block = "challenge2")
  assignation(name, url, course.urls = course.urls, part = part,
    course.names = course.names, toc = toc, texts = texts,
    assign.img = assign.img, assign.link = assign.link, block = block)

#' @rdname assignation
#' @export
assignation_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Complete this assignation:",
    part.name = "part",
    alt       = paste("If you are not a registered user, or if you work",
      "outside of a course, fork [this]({url}){{target=\"_blank\"}}",
      "repository."),
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
    alt       = paste("Si vous \u00eates un utilisateur non enregistr\u00e9 ou",
      "que vous travaillez en dehors d'un cours, faites un \"fork\" de",
      "[ce]({url}){{target=\"_blank\"}} d\u00e9p\u00f4t."),
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

#' @rdname assignation
#' @export
assignation2_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Complete this group assignation:",
    part.name = "part",
    alt       = paste("If you are not a registered user, or if you work",
      "outside of a course, fork [this]({url}){{target=\"_blank\"}}",
      "repository."),
    sub      = "See the explanations in the `README.md`",
    course   = "Group assignation for the students enrolled at the course",
    toc.def  = "Group assignation {name}"
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
assignation2_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "R\u00e9alisez en groupe l'assignation",
    part.name = "partie",
    alt       = paste("Si vous \u00eates un utilisateur non enregistr\u00e9 ou",
      "que vous travaillez en dehors d'un cours, faites un \"fork\" de",
      "[ce]({url}){{target=\"_blank\"}} d\u00e9p\u00f4t."),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = paste("Assignation en groupe pour les \u00e9tudiants",
      "inscrits au cours de"),
    toc.def   = "Assignation en groupe {name}"
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
challenge_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Accept this challenge:",
    part.name = "part",
    alt       = paste("If you are not a registered user, or if you work",
      "outside of a course, fork [this]({url}){{target=\"_blank\"}}",
      "repository."),
    sub      = "See the explanations in the `README.md`",
    course   = "Challenge for the students enrolled at the course",
    toc.def  = "Challenge {name}"
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
challenge_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Relevez le challenge",
    part.name = "partie",
    alt       = paste("Si vous \u00eates un utilisateur non enregistr\u00e9 ou",
      "que vous travaillez en dehors d'un cours, faites un \"fork\" de",
      "[ce]({url}){{target=\"_blank\"}} d\u00e9p\u00f4t."),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = "Challenge pour les \u00e9tudiants inscrits au cours de",
    toc.def   = "Challenge {name}"
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
challenge2_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Accept this group challenge:",
    part.name = "part",
    alt       = paste("If you are not a registered user, or if you work",
      "outside of a course, fork [this]({url}){{target=\"_blank\"}}",
      "repository."),
    sub      = "See the explanations in the `README.md`",
    course   = "Group challenge for the students enrolled at the course",
    toc.def  = "Group challenge {name}"
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
challenge2_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Relevez ce challenge par groupe:",
    part.name = "partie",
    alt       = paste("Si vous \u00eates un utilisateur non enregistr\u00e9 ou",
      "que vous travaillez en dehors d'un cours, faites un \"fork\" de",
      "[ce]({url}){{target=\"_blank\"}} d\u00e9p\u00f4t."),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = paste("Challenge en groupe pour les \u00e9tudiants",
      "inscrits au cours de"),
    toc.def   = "Challenge en groupe {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}
