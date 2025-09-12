#' Record results of learnr exercises in a MongoDB database
#'
#' @param tutorial_id The identifier of the tutorial.
#' @param tutorial_version The version of the tutorial.
#' @param user_id The user identifier for this learnr process.
#' @param event The event that triggers the record, like `exercise_submission`
#' or `question_submission`
#' @param data A JSON field with event-dependent data content. If `NULL`, only
#' a test to see if the database is responding is performed.
#' @param value The new value for user name or email (if not provided, the
#' current value is returned).
#'
#' @description Record tutorial submissions in a MongoDB database. The
#' function is used by learnitdown learnr tutorials and is not for end-users.
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
  bds_dir <- Sys.getenv("LEARNITDOWN_LOCAL_STORAGE")
  if (bds_dir == "")
    bds_dir <- "~/.local/share/R/learnitdown" # Default value
  bds_file <- file.path(bds_dir, "learnr_events")
  debug <- (Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0)

  if (!missing(user_id))
    user_info <- getOption("learnitdown_all_learnr_users", list())[user_id]
  if (is.null(user_info))
    user_info <- getOption("learnitdown_learnr_user")
  if (is.null(user_info) || is.null(user_info$login)) # No login => no records!
    return()

  # In case url.server is defined, we prefer using it, if it works
  is_server_up <- function(url, db) {
    res <- try(mongo(collection = "events", db = db, url = url),
      silent = TRUE)
    !inherits(res, "try-error")
  }
  if (url.server != "" && is_server_up(url = glue(url.server), db = db)) {
    if (isTRUE(debug))
      message("Using server database")
    url <- url.server # Use server URL instead
  } else {
    if (isTRUE(debug) && url.server != "")
      message("Server database not available, using default database")
  }

  # Only test we can open the database... otherwise set the system to only
  # record locally (otherwise, it will be too slow to retest each time).
  if (missing(data) || is.null(data)) {
    # First look of there is an internet connexion
    check_internet_access <- function() {
      ## IANA's test website, but not accessible from everywhere
      #is_online <- function(site = "http://example.com/") {
      #  tryCatch({
      #    readLines(site, n = 1)
      #    TRUE
      #  },
      #    warning = function(w) invokeRestart("muffleWarning"),
      #    error = function(e) FALSE)
      #}
      ## We test two different sites successively
      #res <- is_online("http://www.google.com")
      #if (!res)
      #  res <- is_online("http://www.github.com")
      res <- curl::has_internet()
      if (!res)
        stop("This computer does not seems to have access to the Internet, ", "
        impossible to record events in the database ",
          "(but they are saved on this computer for now).", call. = FALSE)
    }

    res <- try(check_internet_access(), silent = TRUE)
    if (inherits(res, "try-error")) {
      options(learnitdown_learnr_record = FALSE)
      if (debug)
        message("Testing Internet access failed: ", as.character(res))
      return(res)
    }

    # Try to access the database
    m <- try(mongo(collection = "events", db = db, url = glue(url)),
      silent = TRUE)
    if (inherits(m, "try-error")) {
      options(learnitdown_learnr_record = FALSE)
      if (debug)
        message("Testing database access gives an error: ", as.character(m))
      return(m)
    } else {# OK, we can access the database (no insert test, though)
      options(learnitdown_learnr_record = TRUE)
      return("")
    }
  }

  # Add base64 encrypted data in the local file (temporary storage if the
  # database is not available)
  add_file_base64 <- function(entry, file) {
    str <- gsub("\n", "", base64_enc(serialize(entry, NULL)))
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    cat(str, "\n", file = file, append = TRUE)
  }

  # We use the convention that tutorial_version is x.y.z/n
  # where:
  # - x.y.z is actually the version
  # - n is the total number of exercices in the tutorial
  if (grepl("/[0-9]+$", tutorial_version)) {
    version <- sub("/[0-9]+$", "", tutorial_version)
    max <- as.integer(sub("^.*/([0-9]+)$", "\\1", tutorial_version))
  } else {# No indication of the number of exercises
    version <- tutorial_version
    max <- 1 # We indicate 1 by default and score is by exercise only
  }

  # Extract label and correct from data
  label <- data$label
  if (is.null(label)) label <- ""
  data$label <- NULL
  correct <- data$correct
  if (is.null(correct))
    correct <- data$feedback$correct
  if (is.null(correct)) {
    score <- NA
    max <- 0
    grade <- NA
    correct <- ""
  } else {
    # If label contains '_noscore', we don't score this item
    if (grepl("_noscore$", label)) {
      score <- NA
      max <- 0
      grade <- NA
    } else {
      score <- as.integer(correct)
      # PhG: I change grade for a result /1 per exercise!
      grade <- score  #/max
    }
    correct <- as.character(correct)
  }
  data$correct <- NULL

  # Rewrite events with equivalent xAPI verbs
  # see https://rstudio.github.io/learnr/publishing.html
  # and http://xapi.vocab.pub/verbs/index.html
  verb <- switch(event,
    exercise_hint             = "assisted",
    exercise_submitted        = "executed",
    exercise_submission       = "executed", # Not clear which one is correct!
    exercise_result           = "submitted",
    question_submission       = "answered",
    reset_question_submission = "reset",
    video_progress            = "seeked",
    section_skipped           = "progressed",
    section_viewed            = "displayed",
    section_viewed_first_time = "unlocked",
    session_start             = "started",
    session_stop              = "stopped",
    event # Just in case there will be something not in the list
  )
  # If it was question_submission, but nothing in 'correct', then it means that
  # the 'Try again' button was pressed -> verb is reassessed
  if (verb == "answered" && correct == "")
    verb <- "reset"

  # If verb is submitted, but correct == "", then it was 'Run code'
  # => change verb to evaluated
  if (verb == "submitted" && correct == "")
    verb <- "evaluated"

  # If verb is assisted but this is the solution, or a hint with the solution
  # => change verb to revealed
  if (verb == "assisted") {
    if (data$type == "solution")
      verb <- "revealed"
    # If the name ends with _hX and this is hint x-1 (because counted from 0)
    if (isTRUE(grepl("_h[1-9]$", label) && data$type == "hint" &&
      as.integer(data$index) == as.integer(substring(label, nchar(label))) - 1))
      verb <- "revealed"
  }

  # Create an entry for the database, similar to Shiny events
  entry <- list(
    session     = "", # Should we use this?
    date        = format(Sys.time(),
      format = "%Y-%m-%d %H:%M:%OS6", tz = "GMT"),
    type        = "learnr",
    id          = "",
    app         = tutorial_id,
    version     = version,
    user        = user_id,
    login       = if (is.null(user_info$login)) "" else user_info$login, #user_name(),
    email       = if (is.null(user_info$iemail)) "" else user_info$iemail, #tolower(user_email()),
    course      = if (is.null(user_info$icourse)) "" else user_info$icourse,
    institution = if (is.null(user_info$institution)) "" else
      user_info$institution,
    verb        = verb,
    correct     = correct,
    score       = score,
    max         = max,
    grade       = grade,
    label       = label,
    value       = "",
    # jsonlite::toJSON fails because no asJSON method for shiny.tag S3 objects
    #data        = as.character(toJSON(data, auto_unbox = TRUE)),
    data         = as.character(.listToJSON(data))
  )

  db_injected <- FALSE
  use_db <- isTRUE(getOption("learnitdown_learnr_record", default = TRUE))
  if (use_db) {
    m <- try({
      m <- mongo(collection = "events", db = db, url = glue(url))
      m$insert(toJSON(entry, auto_unbox = TRUE))
      if (debug)
        message("Learnr event '", entry$verb, "' inserted into database.")
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
            m$insert(toJSON(unserialize(base64_dec(dat[i])), auto_unbox = TRUE))
          if (debug)
            message(n_pending_events,
              " pending event(s) also inserted in the database")
        }
      }
    }
  }
  # Only get rid of the entry if it was actually injected in the database
  if (!isTRUE(db_injected)) {
    if (debug)
      message("Database not available, saving event '", entry$verb,
        "' locally.")
    add_file_base64(entry, file = bds_file)
  }
}
# Use: options(tutorial.event_recorder = learnitdown::record_learnr)
# To collect these data
#collect_learnr <- function(user, password, server.db = FALSE) {
#  db <- Sys.getenv("MONGO_BASE")
#  url <- glue:glue(Sys.getenv("MONGO_URL")))
#  url.server <- glue:glue(Sys.getenv("MONGO_URL_SERVER")
#  if (isTRUE(server.db))
#    url <- url.server
#  mdb <- mongolite::mongo(collection = "events", db = db, url = url)
#  if (mdb$count())
#    mdb$find()
#}
#my_data <- collect_learnr("user_name", "my_password"); View(my_data)

# Equivalent to svMisc::listToJson, but avoiding dependance to svMisc
.listToJSON <- function(x) {
  if (!is.list(x) && length(x) == 1L)
    return(encodeString(x, quote = "\""))
  x <- lapply(x, .listToJSON)
  x <- if (is.list(x) || length(x) > 1L) {
    nms <- names(x)
    if (is.null(nms)) {
      paste0("[", paste(x, collapse = ","), "]")
    } else {
      paste0("{", paste(paste0(encodeString(make.unique(nms, sep = "#"),
        quote = "\""), ":", x), collapse = ","), "}")
    }
  }
  x
}


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
    message("No learnr activity locally recorded")
    invisible(character(0))
  }
}

#' A default checker that just acknowledges submission
#'
#' This is a simple checker function for learnitdown learnr applications that
#' just indicates to the user that its answer is taken into account.
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

#' Set up a learnitdown Learnr application
#'
#' This function eases the configuration of the learnr document to get user and
#' database info, record events, use grade this and parameterize learnr.
#'
#' @param config The `config()` command to use to get database info.
#' @param sign_in The `sign_in()` command to use to get user info.
#' @param time.limit The maximum time allowed to evaluate R code.
#' @param cap The caption for R code widgets.
#' @param echo Do we echo commands in R chunks?
#' @param comment The prefix added before each line of R chunk output.
#' @param use.gradethis Do we use \{gradethis\}?
#' @param event.recorder The function to use as event recorder. you should
#' probably not change the default value here.
#' @param debug Do we issue additions debugging information?
#'
#' @return Nothing. The function is used to setup the learnr environment.
#' @export
#'
learnitdownLearnrSetup <- function(config, sign_in, time.limit = 60,
cap = "R Code", echo = FALSE, comment = NA, use.gradethis = TRUE,
event.recorder = learnitdown::record_learnr,
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {
  debug <- isTRUE(debug)
  Sys.setenv(LEARNITDOWN_DEBUG = as.integer(debug))
  if (debug)
    message("Learnr application with learnitdown v. ",
      packageVersion("learnitdown"))

  load_lib <- library

  load_lib('learnr')
  load_lib('learnitdown')

  force(config) # Get configuration (database information)
  user <- sign_in # Get user info
  # This in now done in learnitdownLearnrServer()
  #if (is.null(user$login)) {
  #  message("No login, no records!")
  #} else {
  #  message("Recording enabled for ", user$login)
  #}
  options(learnitdown_learnr_user = user)

  if (isTRUE(use.gradethis)) {
    load_lib('gradethis')
    load_lib('testthat')
    gradethis::gradethis_setup()
  }

  # Configure learnr to record events
  options(tutorial.event_recorder = event.recorder)
  tutorial_options(exercise.timelimit = time.limit)
  tutorial_options(exercise.cap = cap)

  # Set general knitr parameters (more suitable ones for learnr)
  load_lib('knitr')
  knitr::opts_chunk$set(echo = echo, comment = comment)
}

#' @rdname learnitdownLearnrSetup
#' @export
#' @param title The Title for the banner.
#' @param text Text to print beneath the title.
#' @param image URL to an image to display in the banner.
#' @param align How is the image aligned: "left" (default), "right", "middle",
#' "top" or "bottom".
#' @param msg.nologin The message to display if no user is logged in.
#' @param msg.login The message to display if a user is logged in (will be
#' followed by the login).
#' @param msg.error The message when an error during recording of activity in
#' the database occurs.
learnitdownLearnrBanner <- function(title, text, image, align = "left",
  msg.nologin = "Anonymous user, no record!",
  msg.login = "Recording activated for ",
  msg.error = "Error recording activity! ") {
  div(id = "learnrBanner", class = "learnrBanner",
    conditionalPanel("output.login == ''",
      div(msg.nologin,
        class = "alert alert-warning", role = "alert", style = "width: 100%;")
    ),
    conditionalPanel("output.login != ''",
      div(msg.login, strong(textOutput("login", inline = TRUE)),
        class = "alert alert-info", role = "alert", style = "width: 100%;")
    ),
    conditionalPanel("output.error != ''",
      div(msg.error, textOutput("error", inline = TRUE),
        class = "alert alert-danger", role = "alert", style = "width: 100%;")
    ),

    # Do we add an image?
    if (!missing(image)) img(src = image, align = align) else "",

    # Do we add a title?
    if (!missing(title)) h1(title) else "",

    # Do we add text?
    if (!missing(text)) text else ""
  )
}

#' @rdname learnitdownLearnrSetup
#' @export
#' @param input The Shiny input.
#' @param output The Shiny output.
#' @param session The Shiny session.
#' @param envir The environment to evaluate the function in (default to the
#'   calling environment).
learnitdownLearnrServer <- function(input, output, session,
    envir = parent.frame()) {
  eval(envir = envir, observe({# Need to evaluate this is the calling environment!
    if (is.null(getOption("learnitdown_learnr_user")$login)) {
      session_user <- session$user
      if (!is.null(session_user) && session_user != "rstudio-connect") {
        # Get more data from the users database, if possible
        url.server <- Sys.getenv("MONGO_URL_SERVER")
        db <- Sys.getenv("MONGO_BASE")
        user <- Sys.getenv("MONGO_USER")
        password <- Sys.getenv("MONGO_PASSWORD")
        if (url.server != "") {
          users <- try(mongo(collection = "users", db = db, url = glue(url.server)),
            silent = TRUE)
          if (inherits(users, "try-error"))
            message("Impossible to connect to the users database.")
          query <- paste0('{ "login": "', session_user, '" }')
          fields <- '{ "login": true, "email": true, "firstname": true, "lastname": true, "iemail": true, "iid": true, "ifirstname": true, "ilastname": true, "icourse": true, "ictitle": true, "iurl": true, "institution": true, "iref": true, "_id": false }'
          query_count <- try(users$count(query), silent = TRUE)
          if (inherits(query_count, "try-error") || !query_count) {
            message("Cannot retrieve '", session_user, "' in the users table.")
            user_info <- list(login = session_user) # Minimal info...
          } else {
            message("User data recovered from the database")
            user_info <- as.list(users$find(query, fields)[1L, ])
          }
          try(users$disconnect(), silent = TRUE)
        }
      } else {# Try getting user data from sign_in or from the URL query string
        user_info <- parseQueryString(session$clientData$url_search)
      }
      # No because it change it for all users!
      #options(learnitdown_learnr_user = user_info)
      # We use another option that stores a list with all connected users data
      all_users <- getOption("learnitdown_all_learnr_users", list())
      all_users[user_info$login] <- user_info
      options(learnitdown_all_learnr_users = all_users)
    } else {# User data already available from sign_in
      user_info <- getOption("learnitdown_learnr_user")
    }
    if (is.null(user_info$login)) {
      message("No login, no records!")
    } else {
      message("Recording enabled for ", user_info$login)
    }
    output$login <- renderText(user_info$login)
    output$error <- renderText(as.character(record_learnr(data = NULL)))
  }))
}
