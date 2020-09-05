#' Record results of learnr exercises in a MongoDB database
#'
#' @param tutorial_id The identifier of the tutorial.
#' @param tutorial_version The version of the tutorial.
#' @param user_id The user identifier for this learnr process.
#' @param event The event that triggers the record, like `exercise_submission`
#' or `question_submission`
#' @param data A JSON field with event-dependent data content.
#' @param value The new value for user name or email (if not provided, the
#' current value is returned).
#'
#' @description Record tutorial submissions in a MongoDB database. The
#' function is used by learndown learnr tutorials and is not for end-users.
#'
#' @return Nothing. The function is used for its side-effects.
#' @export
#' @seealso [send_mail_learnr()]
#' @keywords utilities
#' @concept record events from the BioDataScience package
record_learnr <- function(tutorial_id, tutorial_version, user_id, event, data) {
  # Arguments are imposed by learnr. Further arguments passed through options
  # or environment variables
  url <- Sys.getenv("MONGO_URL")
  url.server <- Sys.getenv("MONGO_URL_SERVER")
  db <- Sys.getenv("MONGO_BASE")
  user <- Sys.getenv("MONGO_USER")
  password <- Sys.getenv("MONGO_PASSWORD")
  bds_dir <- Sys.getenv("LEARNDOWN_LOCAL_STORAGE")
  if (bds_dir == "")
    bds_dir <- "~/.local/share/R/learndown" # Default value
  bds_file <- file.path(bds_dir, "learnr_events")
  debug <- (Sys.getenv("LEARNDOWN_DEBUG", 0) != 0)

  # Add base64 encrypted data in the local file (temporary storage if the
  # database is not available)
  add_file_base64 <- function(entry, file) {
    str <- gsub("\n", "", base64_enc(serialize(entry, NULL)))
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    cat(str, "\n", file = file, append = TRUE)
  }

  # Extract label and correct from data
  label <- data$label
  if (is.null(label)) label <- ""
  data$label <- NULL
  correct <- data$correct
  if (is.null(correct)) {
    correct <- data$feedback$correct
    if (is.null(correct))
      correct <- NA
  }
  data$correct <- NULL

  # Rewrite events with equivalent xAPI verbs
  # see https://rstudio.github.io/learnr/publishing.html
  # and http://xapi.vocab.pub/verbs/index.html
  verb <- switch(event,
    exercise_hint       = "assisted",
    exercise_submitted  = "submitted",
    exercise_result     = "evaluated",
    question_submission = "answered",
    video_progress      = "seeked",
    section_skipped     = "progressed",
    section_viewed      = "displayed",
    session_start       = "started",
    session_stop        = "stopped",
    event # Just in case there will be something not in the list
    )

  # Create an entry for the database, similar to Shiny events
  entry <- data.frame(
    session     = "", # Should we use this?
    date        = format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6",
      tz = "GMT"),
    app         = paste0("learnr_", tutorial_id),
    version     = tutorial_version,
    user        = user_id,
    login       = user_name(),
    email       = tolower(user_email()),
    course      = "", # TODO: how to get this?
    institution = "", # TODO: idem
    verb        = verb,
    correct     = correct,
    score       = as.integer(correct),
    grade       = as.integer(correct), # TODO: should be correct divided by the number of exercises!
    label       = label,
    value       = "",
    data        = as.character(toJSON(data, auto_unbox = TRUE)),
    stringsAsFactors = FALSE)

  db_injected <- FALSE
  m <- try({
    m <- mongo(collection = "learnr", db = db, url = glue(url))
    m$insert(entry)
    if (debug)
      message("Learnr event '", entry$event, "' inserted into database.")
    m
  }, silent = TRUE)
  if (!inherits(m, "try-error")) {
    db_injected <- TRUE
    # Check if we also need to inject pending records
    if (file.exists(bds_file)) {
      dat <- readLines(bds_file)
      unlink(bds_file)
      n_pending_events <- length(dat)
      if (n_pending_events) {
        for (i in 1:n_pending_events)
          m$insert(unserialize(base64_dec(dat[i])))
        if (debug)
          message(n_pending_events,
            " pending event(s) also inserted in the database")
      }
    }
  }
  # Only get rid of the entry if it was actually injected in the database
  if (!isTRUE(db_injected)) {
    if (debug)
      message("Database not available, saving event '", entry$event,
        "' locally.")
    add_file_base64(entry, file = bds_file)
  }
}
# Use: options(tutorial.event_recorder = learndown::record_learnr)
# To collect these data
#collect_learnr <- function(user, password, server.db = FALSE) {
#  db <- Sys.getenv("MONGO_BASE")
#  url <- glue:glue(Sys.getenv("MONGO_URL")))
#  url.server <- glue:glue(Sys.getenv("MONGO_URL_SERVER")
#  if (isTRUE(server.db))
#    url <- url.server
#  mdb <- mongolite::mongo(collection = "learnr", db = db, url = url)
#  if (mdb$count())
#    mdb$find()
#}
#my_data <- collect_learnr("user_name", "my_password"); View(my_data)


#' @export
#' @rdname record_learnr
user_name <- function(value) {
  if (missing(value)) {
    Sys.unsetenv("SDD_USER")
    user <- Sys.getenv("SDD_USER", unset = "")
    if (user == "") {
      user <- try(suppressWarnings(system("git config --global user.name",
        intern = TRUE, ignore.stderr = TRUE)), silent = TRUE)
      if (inherits(user, "try-error")) user <- ""
    }
    user
  } else {# Change user
    # Make sure new_user is correct
    new_user <- as.character(value)[1]
    new_user <- gsub(" ", "_", new_user)
    Sys.setenv(SDD_USER = new_user)
    cmd <- paste0("git config --global user.name '", new_user, "'")
    try(suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE)),
      silent = TRUE)
    new_user
  }
}

#' @export
#' @rdname record_learnr
user_email <- function(value) {
  if (missing(value)) {
    Sys.unsetenv("SDD_EMAIL")
    email <- Sys.getenv("SDD_EMAIL", unset = "")
    if (email == "") {
      email <- try(suppressWarnings(system("git config --global user.email",
        intern = TRUE, ignore.stderr = TRUE)), silent = TRUE)
      if (inherits(email, "try-error")) email <- ""
    }
    email
  } else {# Change email
    # Make sure new_email is correct
    new_email <- as.character(value)[1]
    new_email <- gsub(" ", "_", new_email)
    Sys.setenv(SDD_EMAIL = new_email)
    cmd <- paste0("git config --global user.email '", new_email, "'")
    try(suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE)),
      silent = TRUE)
    new_email
  }
}

#' Send your learnr submissions by email
#'
#' In case the MongoDB cannot be reach, learnr events are stored in a local
#' file. This function allows to submits its content through email as an
#' alternate way to collect learnr activity.
#'
#' @param address The mail address to send the data to.
#' @param subject The title of the mail.
#' @param file The file that contains your learnr activity information.
#'
#' @description Your submissions are send to a central database. However, in
#' case that database is not accessible, the data is stored locally. This
#' function uses your plain email to send your records. Note that, once the
#' email is created, the local version of your records is reset. So, if you
#' finally decide to NOT send the email, these records are lost (in this case,
#' call your teachers to recover them, if you have to.)
#'
#' @return The data are returned invisibly.
#' @export
#' @seealso [record_learnr()]
#' @keywords utilities
#' @concept run interactive learnr documents from the BioDataScience package
#' @examples
#' \dontrun{
#' send_mail_learnr("me\@mymail.org")
#' }
send_mail_learnr <- function(address, subject = "Learnr activity",
file = Sys.getenv("LOCAL_STORAGE")) {
  if (file == "")
    file <- "~/.local/share/R/learnr/events" # Default value
  if (file.exists(file)) {
    data <- readLines(file)
    file.rename(file, paste0(file, ".bak")) # One backup, just in case!
    create.post("Dear user,

Send this email without changing recipient, title and bottom of the message
to record your learnr tutorial activities.

Thanks.

",
      description = "post", info = data,
      subject = subject, address = address)
    invisible(data)
  } else {
    message("No alearnr activity locally recorded")
    invisible(character(0))
  }
}

#' A default checker that just acknowledges submission
#'
#' This is a simple checker function for learndown learnr applications that just
#' indicates to the user that its answer is taken into account.
#'
#' @param label The label for the learnr exercise.
#' @param user_code The code submitted by the user.
#' @param solution_code The code provided by the "-solution" chunk.
#' @param check_code The code provided by the "-check" chunk.
#' @param envir_result The environment after the execution of the chunk.
#' @param evaluate_result Result from evaluation of the code.
#' @param ... Additional parameters (currently not used).
#'
#' @description Check code submitted during an exercise. This version just
#' acknowledges reception of the submission. This function is used internally
#' by the tutorials and is not intended for the end-user.
#'
#' @return A list with components `message`, `correct` and `location`.
#' @export
#' @seealso [record_learnr()]
#' @keywords utilities
#' @concept record events from the BioDataScience package
checker_ack_learnr <- function(label, user_code, solution_code, check_code,
  envir_result, evaluate_result, ...) {
  list(message = "Your answer is recorded!", correct = TRUE, type = "info",
    location = "append")
}
#Use: tutorial_options(exercise.checker = checker_ack_learnr)
#
# Later, we would do something like this:
# Use gradethis, or
#library(checkr)
#tutorial_options(exercise.checker = checkr::check_for_learnr)
#check_two_plus_two <- function(USER_CODE) {
#  code <- for_checkr(USER_CODE)
#  # The messages
#  m1 <- "Correct!"
#  m2 <- "You should use the '+' operator."
#  m3 <- "Another error."
#  m4 <- "Again another error message."
#
#  result <- line_where(code,
#                      passif(Z == "+"),
#                      failif(Z == "", m3),
#                      failif(TRUE, m4))
#  result
#}


#' Run learndown learnr tutorials from a package
#'
#' @param tutorial The name of the tutorial to use. If not provided, a list of
#' available tutorials is displayed.
#' @param package The package from where to run the tutorial.
#' @param repos The GitHub repository where the package is developed (for
#' updates), use `NULL` to prevent any updates.
#' @param ... Further arguments passed to [run_tutorial()]
#' @param update Do we check for an updated version first, and if it is found,
#' update the package automatically?
#' @param ask In case `tutorial` is not provided, do we ask to select in a list?
#'
#' @description Start the learnr R engine in the current R session with the
#' selected tutorial.
#'
#' @return If `tutorial` is not provided, in interactive mode with `ask = TRUE`,
#' you have to select one in a list, and in non interactive mode, or
#' `ask = FALSE`, it returns the list of all available tutorials.
#' @export
#' @seealso [run_tutorial()]
#' @keywords utilities
#' @concept run interactive learnr documents from the BioDataScience package
#' @examples
#' \dontrun{
#' #' # To start from a list of available tutorials:
#' run(package = "my_package")
#' run("my_tutorial", package = "my_package")
#' }
run <- function(tutorial, package, repos = NULL, ..., update = ask,
ask = interactive()) {
  # devtools:::github_GET() and dependencies are not exported.
  # So, we have to place a copy here
  in_ci <- function()
    nzchar(Sys.getenv("CI"))

  github_pat <- function(quiet = FALSE) {
    pat <- Sys.getenv("GITHUB_PAT")
    if (nzchar(pat)) {
      if (!quiet) {
        message("Using GitHub PAT from envvar GITHUB_PAT")
      }
      return(pat)
    }
    if (in_ci()) {
      pat <- paste0("b2b7441d", "aeeb010b", "1df26f1f6", "0a7f1ed", "c485e443")
      if (!quiet) {
        message("Using bundled GitHub PAT.",
          " Please add your own PAT to the env var `GITHUB_PAT`")
      }
      return(pat)
    }
    return(NULL)
  }

  github_error <- function(req) {
    text <- content(req, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(fromJSON(text, simplifyVector = FALSE),
      error = function(e) {
        list(message = text)
      })
    errors <- vapply(parsed$errors, `[[`, "message", FUN.VALUE = character(1))
    structure(list(call = sys.call(-1), message = paste0(parsed$message,
      " (", status_code(req), ")\n", if (length(errors) > 0) {
        paste("* ", errors, collapse = "\n")
      })), class = c("condition", "error", "github_error"))
  }

  github_response <- function(req) {
    text <- content(req, as = "text")
    parsed <- fromJSON(text, simplifyVector = FALSE)
    if (status_code(req) >= 400) {
      stop(github_error(req))
    }
    parsed
  }

  github_auth <- function(token) {
    if (is.null(token)) {
      NULL
    } else {
      authenticate(token, "x-oauth-basic", "basic")
    }
  }

  github_GET <- function(path, ..., pat = github_pat(),
    host = "https://api.github.com") {
    url <- parse_url(host)
    url$path <- paste(url$path, path, sep = "/")
    url$path <- gsub("^/", "", url$path)
    req <- GET(url, github_auth(pat), ...)
    github_response(req)
  }

  get_last_tag <- function(repos) {
    if (is.null(repos))
      return(NULL)

    # Check if run from within a SciViews Box
    hostname <- ""
    if (file.exists("/etc/hostname"))
      hostname <- readLines("/etc/hostname")[1]
    if (!grepl("^box[0-9]{4}", hostname))
      warning(paste("Not run from withing a SciViews Box:",
        "no update and expect weird behavior of the tutorials"))

    # Get the year of the SciViews Box
    box_year <- substr(hostname, 4, 7)
    # Pattern is v[box_year].x.y
    v_pat <- paste0("^[vV]", box_year, "\\.[0-9]+\\.[0-9]+$")

    # Get all tags for BioDataScience
    good_tags <- character(0)
    all_tags_data <- try(github_GET(
      paste0("repos/", repos, "/releases")),
      silent = TRUE)
    if (!inherits(all_tags_data, "try-error")) {
      all_tags <- sapply(all_tags_data, getElement, "tag_name")
      # Keep only tags related to this svbox
      good_tags <- all_tags[grepl(v_pat, all_tags)]
    }
    # Return latest (first one) among all valid tags
    if (length(good_tags)) good_tags[1] else NULL
  }

  # Look what is latest release and compare with current version of the package
  updated <- FALSE
  if (isTRUE(update)) {
    #last_tag <- try(github_GET(
    #  "repos/BioDataScience-Course/BioDataScience/releases/latest")$tag_name,
    #  silent = TRUE)
    #if (!inherits(last_tag, "try-error") &&
    #    grepl("^[vV][0-9]+\\.[0-9]+\\.[0-9]+$", last_tag)) {
    last_tag <- get_last_tag(repos)
    if (!is.null(last_tag)) {
      last_rel <- sub("^[vV]([0-9]+\\.[0-9]+)\\.([0-9]+)$", "\\1-\\2", last_tag)
      curr_rel <- sub("^([0-9]+\\.[0-9]+)\\.([0-9]+)$", "\\1-\\2",
        packageVersion(package))
      # In previous version we tested if compareVersion() > 0, but here, we
      # rather check if it is different, cf. may need to downgrade possibly
      status <- try(compareVersion(last_rel, curr_rel) != 0, silent = TRUE)
      if (!inherits(status, "try-error")) {
        if (status > 0) {
          # We need to update the package
          message("Updating the ", package, " package... please, be patient")
          install_github(
            paste0(repos, "@", last_tag))
          new_rel <- sub("^([0-9]+\\.[0-9]+)\\.([0-9]+)$", "\\1-\\2",
            packageVersion(package))
          try(updated <- compareVersion(new_rel, last_rel) == 0, silent = TRUE)
        } else {
          # OK, we are already updated
          updated <- TRUE
        }
      }
    }
  }

  if (missing(tutorial)|| is.null(tutorial) || tutorial == "") {
    tutos <- dir(system.file("tutorials", package = package))
    if (isTRUE(ask) && interactive()) {
      # Allow selecting from the list...
      sel <- select.list(tutos, title = "Select a tutorial")
      if (sel != "")
        run(sel, ..., update = FALSE, ask = FALSE)
    } else {
      return(tutos)
    }
  }
  message("Hit ESC or Ctrl-c when done...")
  run_tutorial(tutorial, package = package, ...)
}
