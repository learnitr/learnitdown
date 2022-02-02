#' Insert a GitHub (Classroom) assignment or challenge in the document
#'
#' Insert a block of class `assign`, `assign2`, `challenge` or `challenge2` with
#' data related to a GitHub (Classroom) assignment.
#'
#' @param name The name of the assignment or the challenge (usually the same as
#' the name as the GitHub Classroom assignment).
#' @param url The URL of the assignment or challenge (could be a named list for
#' different courses).
#' @param alturl An alternate URL to propose to external users not registered in
#' a course. If not provided, it is the same as `url=`.
#' @param course.ids Named vector with the Classroom identifiers for the
#' assignments for each course and also possibly, for each group.
#' @param course.urls Named vector with the Classroom URLS for each course, and
#' also possibly for groups.
#' Names are the course identifiers in Moodle, or the groups defined for the
#' users. If `NULL`, no course-specific sections are added.
#' @param course.starts Named vector (same logic as for `course.urls=`) with
#' starting dates (characters like "YYYY-mm-dd HH:MM:SS"). A corresponding entry
#' in `course.urls=` is required, or the dates will be ignored.
#' @param course.ends Named vector (same logic as for `course.urls=`) with
#' ending dates (characters like "YYYY-mm-dd HH:MM:SS"). A corresponding entry
#' in `course.urls=` is required, or the dates will be ignored.
#' @param part If the assignment presents several parts in the `README.md` file
#' (because the same assignment is used at different places), indicate the part
#' here. Otherwise, leave the default value `NULL` if there are no parts.
#' @param course.names A named character vector with the name of the course.
#' Names used must be the same as for `course.urls`, but there may be more here,
#' and not necessarily in the same order.
#' @param toc The exercise table of content (ex-toc) label. If `toc = ""`
#' (default), a generic label is calculated using `toc.def` from `texts`. To not
#' display the exercise in the table of content, use `toc = NULL`.
#' @param clone Should the exercise be listed for cloning the repositories
#' (`TRUE` by default)? If `TRUE`, an entry is added in the list of assignments
#' whose repositories should be cloned.
#' @param level The difficulty level (1 = easiest, 2 = more difficult, ...)
#' @param n The number of students per project (by default, n = 1 for individual
#' assignments and 2 for group assignments).
#' @param type The type of exercise. By default, it is `"ind. github"` or
#' `"ind. challenge"` if n = 1, or `"group github"`/`"group challenge"`
#' otherwise.
#' @param acad_year The academic year (e.g., 2021-2022).
#' @param term The term (e.g., Q1, Q2, Q3).
#' @param texts Various sentences used to construct the assignment bloc. You
#' can make a call to `assignment_en()` or `assignment_fr()` as a basis
#' and modify only the sentences you want.
#' @param assign.img The relative path to the image to use before the label in
#' the ex-toc.
#' @param assign.link The link to the assignment help page (when the user clicks on
#' the image in the ex-toc).
#' @param block The class of the div, or the LaTeX environment to use for the
#' assignment block.
#' @param template The template file to use for the URL redirection page
#' (currently only assignment_en.html or assignment_fr.html).
#' @param baseurl The base URL for the web site.
#' @param title The title of the block.
#' @param part.name The word to use for "part".
#' @param alt The text to display for alternate access to the repository (for
#' non-registered users). Use a string following the [glue()] syntax to replace
#' variables.
#' @param sub The text that appears at the bottom of the assignment block.
#' @param course The text for items corresponding to courses.
#' @param toc.def The default ex-toc label (using [glue()] syntax).
#'
#' @return Markdown code that generates the GitHub Classroom assignment block.
#' It is most conveniently used inside an R chunk in your R Markdown document.
#' If you do not want to break your code chunks inside RStudio, you may use
#' something like `if (exists("assignment")) assignment(...)`.
#' @export
#'
#' @details
#' If the URL contains several entries, names are used to create show/hide divs
#' according to the `icourse` user information.
assignment <- function(name, url, alturl = url, course.ids = NULL,
course.urls = NULL, course.starts = NULL, course.ends = NULL, part = NULL,
course.names = c(course1 = "Data Science"), toc = "", clone = TRUE, level = 3,
n = 1, type = if (n == 1) "ind. github" else "group github",
acad_year = "", term = "",
texts = assignment_en(), assign.img = "images/list-assign.png",
assign.link = "github_assignment", block = "assign",
template = "assignment_en.html", baseurl = "/") {
  # Make sure n is a valid number
  n <- as.integer(n[1])
  if (!length(n) || n < 1)
    n <- 1

  # Manage parts
  if (is.null(part)) {
    anchor <- name
    part2 <- NA
    part <- ""
  } else {
    # Anchor is a combination of name and part to separate different parts
    anchor <- paste0(name, part)
    part2 <- part
    part <- paste(",", texts$part.name, part)
  }

  # Create an HTML page that redirects the URLs given the context
  # (logged user at the right course, and in the right time range)
  # Also populate a CSV table with data about the assignment
  output_dir <- .get_output_dir()
  ex_dir <- file.path(output_dir, "ex")
  dir.create(ex_dir, showWarnings = FALSE)
  file_basename <- paste0("assignment_", URLencode(anchor, reserved = TRUE))
  file_url <- paste0("ex/", file_basename, ".html")
  file_html <- file.path(ex_dir, paste0(file_basename, ".html"))
  file_csv <- file.path(ex_dir, paste0(file_basename, ".csv"))

  # Populate a data frame with assignments data
  ids <- if (is.null(course.ids)) c(' ' = NA) else course.ids
  urls <- if (is.null(course.urls)) NA else course.urls
  starts <- if (is.null(course.starts)) NA else course.starts
  ends <- if (is.null(course.ends)) NA else course.ends
  ex_data <- data.frame(
    app = name,
    type = type,
    icourse = names(urls),
    course = substring(name, 1, 1),
    acad_year = acad_year,
    term = term,
    module = substring(name, 1, 3),
    assignment = as.character(ids),
    template = url,
    url = as.character(urls[names(ids)]),
    alt_url = alturl,
    start = as.character(starts[names(ids)]),
    end = as.character(ends[names(ids)]),
    part = part2,
    toc = !is.null(toc),
    clone = as.logical(clone),
    n = n,
    level = level)
  # Write this file in ex_dir
  write.csv(ex_data, file_csv, row.names = FALSE)

  # Copy the GitHub Classroom image there
  ghc_image <- file.path(ex_dir, "github_classroom.jpg")
  if (!file.exists(ghc_image))
    file.copy(system.file("assignments", "github_classroom.jpg",
      package = "learnitdown"), ghc_image)
  # Read the HTML template
  temp <- readLines(system.file("assignments", template,
    package = "learnitdown"), encoding = "UTF-8")
  temp <- paste(temp, collapse = "\n")
  # Compute course_params
  course_params <- ""
  if (nrow(ex_data)) {
    for (i in 1:nrow(ex_data)) {
      ex <- ex_data[i, ]
      ex_course <- ex$icourse
      ex_url <- ex$url
      ex_start <- ex$start
      if (is.na(ex_start)) {
        ex_start <- "null"
      } else {
        ex_start <- paste0('new Date("', .format_js_time(ex_start), '")')
      }
      ex_end <- ex$end
      if (is.na(ex_end)) {
        ex_end <- "null"
      } else {
        ex_end <- paste0('new Date("', .format_js_time(ex_end), '")')
      }
      par <- glue("          '{ex_course}': {{
            'url':   \"{ex_url}\",
            'start': {ex_start},
            'end':   {ex_end}
          }}")
      if (course_params == "") {
        course_params <- par
      } else {
        course_params <- paste0(course_params, ",\n", par)
      }
    }
  }
  # Interpolate fields using glue
  temp <- glue(temp, .open = "{{", .close = "}}")
  # Write file in ex_dir
  writeLines(temp, file_html)

  # Create a toc entry
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

  # Compute the Markdown block for this assignment
  # Use course-specific sections to indicate normal starting and
  # ending dates for the assignment
  if (is.null(course.urls) || !length(course.urls)) {
    course_text <- ""
  } else {# for each course, create one entry
    course_ids <- names(course.urls)
    course_names <- course.names[course_ids]
    course_text <- ""
    for (i in 1:length(course_ids)) {
      course_id <- course_ids[i]
      course_url <- course.urls[course_id]
      course_name <- course_names[i]
      course_end <- course.ends[course_id]
      if (is.null(course_end) || is.na(course_end) || course_end == "" ||
        is.na(course_name)) {# No final date, or not a course (= student group)
        course_desc <- ""
      } else {
        course_desc <- glue::glue(texts$course)
        course_text <- paste0(course_text, "\n",
          glue::glue("\n::: {{ .{course_id} }}
**{course_desc}**
\n:::\n\n"))
      }
    }
  }

  # We don't use the template url in the box, but point to our
  # assignment_<name>.html page
  url <- file_url
  alt_text <- glue::glue(texts$alt)

  glue::glue("\n\\BeginKnitrBlock{{{block}}}<div class=\"{block}\">
{texts$title} **[{name}]{{#{anchor} }}{part}**.

{course_text}

{alt_text}

*{texts$sub}{part}.*
</div>\\EndKnitrBlock{{{block}}}\n\n")
}

#' @rdname assignment
#' @export
assignment2 <- function(name, url, alturl = url, course.ids = NULL,
course.urls = NULL, course.starts = NULL, course.ends = NULL, part = NULL,
course.names = c(course1 = "Data Science"), toc = "", clone = TRUE, level = 3,
n = 1, type = if (n == 1) "ind. github" else "group github",
acad_year = "", term = "",
texts = assignment2_en(), assign.img = "images/list-assign2.png",
assign.link = "github_assignment", block = "assign2",
template = "assignment_en.html", baseurl = "/")
  assignment(name, url, course.ids = course.ids, course.urls = course.urls,
    course.starts = course.starts, course.ends = course.ends, part = part,
    course.names = course.names, toc = toc, clone = clone, level = level, n = n,
    type = type, acad_year = acad_year, term = term, texts = texts,
    assign.img = assign.img, assign.link = assign.link, block = block,
    template = template, baseurl = baseurl)

#' @rdname assignment
#' @export
challenge <- function(name, url, alturl = url, course.ids = NULL,
course.urls = NULL, course.starts = NULL, course.ends = NULL, part = NULL,
course.names = c(course1 = "Data Science"), toc = "", clone = TRUE, level = 3,
n = 1, type = if (n == 1) "ind. challenge" else "group challenge",
acad_year = "", term = "",
texts = challenge_en(), assign.img = "images/list-challenge.png",
assign.link = "github_challenge", block = "challenge",
template = "assignment_en.html", baseurl = "/")
  assignment(name, url, course.ids = course.ids, course.urls = course.urls,
    course.starts = course.starts, course.ends = course.ends, part = part,
    course.names = course.names, toc = toc, clone = clone, level = level, n = n,
    type = type, acad_year = acad_year, term = term, texts = texts,
    assign.img = assign.img, assign.link = assign.link, block = block,
    template = template, baseurl = baseurl)

#' @rdname assignment
#' @export
challenge2 <- function(name, url, alturl = url, course.ids = NULL,
course.urls = NULL, course.starts = NULL, course.ends = NULL, part = NULL,
course.names = c(course1 = "Data Science"), toc = "", clone = TRUE, level = 3,
  n = 1, type = if (n == 1) "ind. challenge" else "group challenge",
acad_year = "", term = "",
texts = challenge2_en(), assign.img = "images/list-challenge2.png",
assign.link = "github_challenge", block = "challenge2",
template = "assignment_en.html", baseurl = "/")
  assignment(name, url, course.ids = course.ids, course.urls = course.urls,
    course.starts = course.starts, course.ends = course.ends, part = part,
    course.names = course.names, toc = toc, clone = clone, level = level, n = n,
    type = type, acad_year = acad_year, term = term, texts = texts,
    assign.img = assign.img, assign.link = assign.link, block = block,
    template = template, baseurl = baseurl)

#' @rdname assignment
#' @export
assignment_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Complete this assignment:",
    part.name = "part",
    alt       = "**[Start it in GitHub Classroom]({url}){{target=\"_blank\"}}**",
    sub      = "See the explanations in the `README.md`",
    course   = paste("Individual assignment for students enrolled in",
      "{course_name} that must be completed before {course_end}."),
    toc.def  = "Individual assignment {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignment
#' @export
assignment_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "R\u00e9alisez le travail",
    part.name = "partie",
    alt       = paste0("**[Initiez votre projet GitHub Classroom]",
      "({url}){{target=\"_blank\"}}**"),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = paste("Travail individuel pour les \u00e9tudiants inscrits au",
      "cours {course_name} \u00e0 terminer avant le {course_end}."),
    toc.def   = "Travail individuel {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignment
#' @export
assignment2_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Complete this group assignment:",
    part.name = "part",
    alt       = "**[Start it in GitHub Classroom]({url}){{target=\"_blank\"}}**",
    sub      = "See the explanations in the `README.md`",
    course   = paste("Assignment in group of {n} for the students",
      "enrolled in the course {course_name} that must be completed before",
      "{course_end}."),
    toc.def  = "Group assignment {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignment
#' @export
assignment2_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "R\u00e9alisez en groupe le travail",
    part.name = "partie",
    alt       = paste0("**[Initiez votre projet GitHub Classroom]",
      "({url}){{target=\"_blank\"}}**"),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = paste("Travail en groupe de {n} pour les \u00e9tudiants",
      "inscrits au cours de {course_name} \u00e0 terminer avant le {course_end}."),
    toc.def   = "Travail de groupe {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignment
#' @export
challenge_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Accept this challenge:",
    part.name = "part",
    alt       = "**[Start it in GitHub Classroom]({url}){{target=\"_blank\"}}**",
    sub      = "See the explanations in the `README.md`",
    course   = paste("Individual challenge for the students enrolled in the",
      "course {course_name} to be completed before {course_end}."),
    toc.def  = "Individual challenge {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignment
#' @export
challenge_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Relevez le challenge",
    part.name = "partie",
    alt       = paste0("**[Initiez le projet GitHub Classroom de ce challenge]",
      "({url}){{target=\"_blank\"}}**"),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = paste("Challenge individuel pour les \u00e9tudiants",
      "inscrits au cours de {course_name} \u00e0 terminer avant le {course_end}."),
    toc.def   = "Challenge individuel {name}"
  )
  if (!missing(title)) texts$title <- title
  if (!missing(part.name)) texts$part.name <- part.name
  if (!missing(alt)) texts$alt <- alt
  if (!missing(sub)) texts$sub <- sub
  if (!missing(course)) texts$course <- course
  if (!missing(toc.def)) texts$toc.def <- toc.def

  texts
}

#' @rdname assignment
#' @export
challenge2_en <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Accept this group challenge:",
    part.name = "part",
    alt       = "**[Start it in GitHub Classroom]({url}){{target=\"_blank\"}}**",
    sub      = "See the explanations in the `README.md`",
    course   = paste("Challenge in group of (n) for the students enrolled",
      "in the course {course_name} to be completed before {course_end}."),
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

#' @rdname assignment
#' @export
challenge2_fr <- function(title, part.name, alt, sub, course, toc.def) {
  texts <- list(
    title     = "Relevez ce challenge par groupe:",
    part.name = "partie",
    alt       = paste0("**[Initiez le projet GitHub Classroom de ce challenge]",
      "({url}){{target=\"_blank\"}}**"),
    sub       = "Voyez les explications dans le fichier `README.md`",
    course    = paste("Challenge en groupe de {n} pour les \u00e9tudiants",
      "inscrits au cours de {course_name} \u00e0 terminer avant le {course_end}."),
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
