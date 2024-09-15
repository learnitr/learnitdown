#' Read shinylogs log data and format them in a data.frame
#'
#' Learnitdown Shiny applications are a special kind of Shiny applications that
#' logs events and check results. It uses the `shinylogs` package to log Shiny
#' events and [read_shinylogs()] reads such logs and convert their data into a
#' format that is suitable to include, say in a MongoDB database.
#'
#' @param file The path to the RDS file that contains the shinylogs log data.
#' @param version The version of the Shiny application. Version is not recorded
#' by shinylogs, so, we must provide it here.
#' @param log.errors Do we record errors too?
#' @param log.outputs Do we record outputs too (note that results are recorded
#' with inputs)?
#'
#' @return A data frame with the different events logged (one per line).
#' @export
#'
#' @seealso [record_shiny()], [trackEvents()]
read_shinylogs <- function(file, version = "0",
  log.errors = TRUE, log.outputs = FALSE) {

  if (!grepl("\\.rds$", file))
    stop("read only rds shinylogs file")

  if (!file.exists(file))
    stop("file not found")

  logs <- read_rds_logs(file)

  # Initialize the data.frame with start and stop events
  session <- logs[["session"]]
  user_data <- try(fromJSON(session$user), silent = TRUE)
  if (inherits(user_data, "try-error"))
    user_data <- list(user = "", login = "", iemail = "",
      icourse = "", institution = "", pathname = session$app)
  user <- user_data$user
  if (is.null(user) || !length(user) || user == "")
    user <- Sys.info()['user']
  if (is.null(user) || !length(user))
    user <- ""
  if (is.null(user_data$login) || !length(user_data$login))
    user_data$login <- ""
  if (is.null(user_data$iemail) || !length(user_data$iemail))
    user_data$iemail <- ""
  if (is.null(user_data$icourse) || !length(user_data$icourse))
    user_data$icourse <- ""
  if (is.null(user_data$institution) || !length(user_data$institution))
    user_data$institution <- ""

  app_name <- session$app
  # In the case of RStudio Connect, we just get "app", not very informative
  # => in that case, try to get something more meaninful from url_pathname
  if (app_name == "app")
    app_name <- basename(user_data$pathname)

  if (!length(version)) version <- "" # In case version is missing
  common_data <- list(
    app         = app_name,
    version     = as.character(version),
    user        = as.character(user),
    login       = user_data$login,
    email       = tolower(user_data$iemail),
    course      = user_data$icourse,
    institution = user_data$institution
  )

  events <- data.frame(
    sessionid = c(session$sessionid, session$sessionid),
    name = c("", ""),
    timestamp = c(session$server_connected, session$server_disconnected),
    value = c(session$user, ""), # Complete user info only in the start event
    type = c("session.id", "session.id"),
    binding = c("", ""),
    state = c("started", "stopped"),
    stringsAsFactors = FALSE)

  # Inputs
  if (length(logs$inputs)) {
    inputs <- logs[["inputs"]]
    inputs$state <- "interacted"
    inputs$value <-  as.character(inputs$value)
    events <- rbind(events, inputs)
  }

  # Errors
  if (isTRUE(log.errors) && length(logs$errors)) {
    errors <- logs[["errors"]]
    errors$value <- as.character(errors$error)
    errors$error <- NULL
    errors$type <- "text"
    errors$binding <- ""
    errors$state <- "debugged"
    events <- rbind(events, errors)
  }

  # Outputs
  if (isTRUE(log.outputs) && length(logs$outputs)) {
    outputs <- logs[["outputs"]]
    outputs$value <- as.character(outputs$value)
    outputs$state <- "computed"
    outputs$type <- ""
    outputs <- outputs[, c("sessionid", "name", "timestamp", "value", "type",
      "binding", "state")]
    events <- rbind(events, outputs)
  }

  # Now we combine common_data and events into a data.frame similar to what
  # we got from learnr applications
  res <- try(data.frame(
    session     = events$sessionid,
    date        = format(events$timestamp, format = "%Y-%m-%d %H:%M:%OS6",
      tz = "GMT"),
    type        = "shiny",
    id          = "",
    app         = common_data$app,
    version     = common_data$version,
    user        = common_data$user,
    login       = common_data$login,
    email       = common_data$email,
    course      = common_data$course,
    institution = common_data$institution,
    verb        = events$state,
    correct     = "", # We will rework this for results later on
    score       = NA_integer_, # Idem
    max         = 0L, # Idem
    grade       = NA_integer_, # Idem
    label       = events$name,
    value       = events$value,
    data        = paste0('{"type":"', events$type, '","binding":"',
      events$binding, '"}'),
    stringsAsFactors = FALSE), silent = TRUE)
  if (inherits(res, "try-error")) {
    # Something was wrong with the collection of data (this happens, e.g., when
    # the computer is not connected to the Internet)
    return(res)
  }

  # Filter out .clientdata_output_... labels we don't want to keep
  keep_event <- substring(res$label, 1L, 19L) != ".clientdata_output_"
  res <- res[keep_event, ]

  # Rework submit and quit events
  is_submit <- res$label == "learnitdown_submit_"
  if (any(is_submit)) {
    res$verb[is_submit] <- "submitted"
    res$label[is_submit] <- ""
  }
  is_quit <- res$label == "learnitdown_quit_"
  if (any(is_quit)) {
    res$verb[is_quit] <- "exited"
    res$label[is_quit] <- ""
  }

  # Rework result events
  is_result <- res$label == "learnitdown_result_"
  if (any(is_result)) {
    res$verb[is_result] <- "evaluated"
    res$label[is_result] <- ""
    results <- res$value[is_result]
    # We strip correct (TRUE/FALSE) out and put it in the correct column
    values <- character(0)
    correct <- character(0)
    score <- numeric(0)
    max <- numeric(0)
    for (i in 1:length(results)) {
      # We want to protect here against wrong entries!
      value <- try(fromJSON(results[i]), silent = TRUE)
      if (inherits(value, "try-error")) {
        correct[i] <- "NA"
        score[i] <- NA
        max[i] <- 0
        values[i] <- results[i]
      } else {
        correct[i] <- value$correct
        value$correct <- NULL
        score[i] <- value$score
        value$score <- NULL
        max[i] <- value$max
        value$max <- NULL
        values[i] <- as.character(toJSON(value, auto_unbox = TRUE))
      }
    }
    res$correct[is_result] <- correct
    res$score[is_result] <- score
    res$max[is_result] <- max
    res$grade[is_result] <- score/max
    res$value[is_result] <- values
  } else {
    # We want at least one result, so, we add one with correct == "NA" now
    fake_result <- data.frame(
      session     = session$sessionid,
      date        = format(session$server_disconnected,
        format = "%Y-%m-%d %H:%M:%OS6", tz = "GMT"),
      type        = "shiny",
      id          = "",
      app         = common_data$app,
      version     = common_data$version,
      user        = common_data$user,
      login       = common_data$login,
      email       = common_data$email,
      course      = common_data$course,
      institution = common_data$institution,
      verb        = "evaluated",
      correct     = "NA",
      score       = NA,
      max         = 0,
      grade       = NA,
      label       = "",
      value       = "",
      data        = '{type":"","binding":"shiny.textInput"}',
      stringsAsFactors = FALSE)
    res <- rbind(res, fake_result)
  }

  # Sort by date
  res <- res[order(res$date), ]
  res
}

.record_shinylogs <- function(file, url, db, collection = "events",
version = "0", log.errors = TRUE, log.outputs = FALSE,
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {

  if (!file.exists(file))
    return(TRUE)

  dat <- read_shinylogs(file, version = version,
    log.errors = log.errors, log.outputs = log.outputs)

  if (inherits(dat, "try-error")) {
    if (debug)
      message("Error while reading log file ", file, ": ", dat)
    return(structure(FALSE, error = dat))
  }
  if (debug)
    message(NROW(dat), " events found in ", file)

  res <- try({
    if (debug)
      message("Connecting to database...")
    m <- mongo(collection = collection, db = db, url = url)
    if (debug)
      message("Inserting events into the database...")
    m$insert(dat, na = "null", auto_unbox = TRUE, force = TRUE)
    if (debug)
      message("...done!")
  }, silent = TRUE)

  if (!inherits(res, "try-error")) {
    unlink(file)
    if (debug)
      message("Successful transfer of data from ", file,
        " to the MongoDB database")
    TRUE
  } else {
    if (debug)
      message("Error while transferring log file ", file, ": ", res)
    structure(FALSE, error = res)
  }
}

# Test:
# my_file <- "logs/shinylogs_shinylogs_app02_1598274821730014000.rds"
# user <- Sys.getenv("MONGO_USER")
# password <- Sys.getenv("MONGO_PASSWORD")
# my_url <- glue::glue(Sys.getenv("MONGO_URL"))
# my_db <- Sys.getenv("MONGO_BASE")
# .record_shinylogs(my_file, url = my_url, db = my_db, version = "1.0.0")

#' Record Shiny events in a MongoDB database
#'
#' Given a path that contains `shinylogs` events in .rds format, read these
#' events and transfer them into a MongoDB database.
#'
#' @param path The directory that contains shinylogs .rds files
#' @param url The mongodb url.
#' @param db The database name.
#' @param collection The name of the collection where to insert the documents.
#' @param version The version of the running Shiny application.
#' @param log.errors Do we record errors too?
#' @param log.outputs Do we record outputs too (note that results are recorded
#' with inputs)?
#' @param drop.dir If `TRUE` and path is empty at the end of the process, drop
#' the logs directory.
#' @param debug Do we debug the events recording by issuing extra messages? By
#' Default the value in the environment variable `LEARNITDOWN_DEBUG` is used and
#' debugging will be done if this value is different to `0`.
#'
#' @return `TRUE`if there where log files to export, `FALSE` otherwise.
#' @export
#'
#' @seealso [read_shinylogs()], [trackEvents()]
record_shiny <- function(path, url, db, collection = "events",
version = "0", log.errors = TRUE, log.outputs = FALSE, drop.dir = FALSE,
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {
  debug <- isTRUE(debug)
  log_files <- dir(path, pattern = "\\.rds$", full.names = TRUE)
  if (length(log_files)) {
    if (debug)
      message(".rds log file(s) found in ", path, ": ", length(log_files))
    answer <- TRUE
    for (file in log_files) {
      res <- .record_shinylogs(file, url = url, db = db,
        collection = collection, version = version, log.errors = log.errors,
        log.outputs = log.outputs, debug = debug)
      if (!res && answer)# Return first error found
        answer <- res
      # Delete the file
      file.remove(file)
    }
  } else {
    if (debug)
      message("No .rds log file found in ", path)
    return(TRUE)
  }
  if (isTRUE(drop.dir)) {
    if (debug) {
      log_files_remaining <- dir(path, pattern = "\\.rds$")
      message("Remaining log files after transfer to MongoDB: ",
        length(log_files_remaining))
    }
    # Do not do this: delete instead each file that is processed one by one above
    #suppressWarnings(file.remove(path)) # non-empty dir not deleted with warning
  }
  answer
}

#' Create and manage learnitdown Shiny applications
#'
#' A learnitdown Shiny application is an application whose events (start, stop,
#' inputs, outputs, errors, result, quit) are recorded. It also provides a
#' 'Submit Answer' and a 'Quit' buttons that manage to check the answer provided
#' by the user and to close the application cleanly.
#'
#' @param title The title of the Shiny application
#' @param windowTitle The title of the window that holds the Shiny application,
#' by default, it is the same as `title`. If `title = NULL`, no title is added.
#' @param version The version number (in a string character format) of the Shiny
#' application to use in the events logger.
#' @param inputId The identifier of the button ("learnitdown_submit_" or
#' "learnitdown_quit_").
#' @param label The button text ("Submit" or "Quit").
#' @param class The bootstrap class of the button.
#' @param ... Further arguments passed to [shiny::actionButton()].
#'
#' @return The code to be inserted at the beginning of the Shiny application UI
#' for [learnitdownShiny()] or where the buttons should be located for the other
#' function.
#' @export
#'
#' @seealso [trackEvents()], [record_shiny()]
#'
#' @examples
#' learnitdownShiny("My title")
learnitdownShiny <- function(title, windowTitle = title) {
  tagList(
    # Required initialisation
    useToastr(),

    # Add a level 4 title (if not NULL)
    if (is.null(title)) NULL else
      tagList(tags$head(tags$title(windowTitle)), h4(title)),

    # Add an hidden input field to contain learnitdown result
    conditionalPanel(condition = "false", textInput("learnitdown_result_", ""))
  )
}

#' @export
#' @rdname learnitdownShiny
learnitdownShinyVersion <- function(version)
  options(learnitdown.shiny.version = as.character(version))

#' @export
#' @rdname learnitdownShiny
submitAnswerButton <- function(inputId = "learnitdown_submit_",
label = "Submit", class = "btn-primary", ...)
  actionButton(inputId, label = label, class = class, ...)

#' @export
#' @rdname learnitdownShiny
quitButton <- function(inputId = "learnitdown_quit_", label = "Quit",
class = "btn-secondary", ...)
  actionButton(inputId, label = label, class = class, ...)

#' @export
#' @rdname learnitdownShiny
submitQuitButtons <- function() {
  fluidRow(
    submitAnswerButton(),
    quitButton()
  )
}

#' Track events, the submit or the quit buttons
#'
#' These functions provide the required code to be inserted in the server part
#' of a Shiny application to track events (start, stop, inputs, outputs, errors,
#' result or quit), to check the answer when the user clicks on the submit
#' button and to cleanly close the application when the user clicks on the quit
#' button.
#'
#' @param session The current Shiny `session`.
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param sign_in.fun The function that can get user info from the
#' [sign_in()], or `NULL` by default to disable using local user data.
#' @param url The URL to reach the MongoDB database. By default, it is read from
#' the `MONGO_URL` environment variable.
#' @param url.server The URL to reach the MongoDB database. By default, it is
#' read from the `MONGO_URL_SERVER` environment variable.
#' @param db The database to populate in the MongoDB database. By default, it is
#' read from the `MONGO_BASE` environment variable.
#' @param user The user login to the MongoDB database. By default, it is
#' read from the `MONGO_USER` environment variable.
#' @param password The password to access the MongoDB database. By default, it
#' is read from the `MONGO_PASSWORD` environment variable.
#' @param version The version of the current Shiny application. By default, it
#' is the `learnitdown.shiny.version` option, as set by
#' [learnitdownShinyVersion()].
#' @param path The path where the temporary `shinylogs` log files are stored. By
#' default, it is set to the `LEARNITDOWN_LOCAL_STORAGE` environment variable,
#' or to `shiny_logs` subdirectory of the application if not defined. If that
#' directory is not writable, a temporary directory is used instead.
#' @param log.errors Do we log error events (yes by default)?
#' @param log.outputs Do we log output events (no by default)?
#' @param drop.dir Do we erase the directory indicated by `path =` if it is
#' empty at the end of the process (yes by default).
#' @param debug Do we debug recording of events using extra messages? By
#' default, it is the value of the environment variable `LEARNITDOWN_DEBUG`, and
#' debugging is activated when that value is different to `0`.
#' @param solution The correct solution as a named list. Names are the
#' application inputs to check and their values are the correct values. The
#' current state of these inputs will be compared against the solution, and the
#' `message.success` or `message.error` is displayed accordingly. Also a result
#' event is triggered with the `correct` field set to `TRUE` or `FALSE`
#' accordingly. If `solution = NULL`, then the answer is considered to be always
#' correct (use this if you just want to indicate that the user's answer is
#' recorded in the database). For numeric values, use `c(min = x, max = y) to
#' check if values are within a range.
#' @param answer A named list of the answer data to check against solution. This
#' object is provided by `trackSubmit()`.
#' @param max_score The highest score value that could be attributed if the
#' exercise is correctly done. By default, it is `NULL` meaning that the higher
#' score would be equal to the number of items to check in `solution`.
#' @param score.txt The word to use for "score" ("Score" by default).
#' @param check.solution The function to use to check if solution provided by
#' a Shiny application is correct or not. By default, it is
#' `check_shiny_solution()`.
#' @param comment A string with a comment to append to the value of the result
#' event.
#' @param message.success The message to display is the answer is correct
#' ("Correct" by default).
#' @param message.error The message to display if the answer is wrong
#' ("Incorrect" by default).
#' @param delay The time to wait before we close the Shiny application in sec
#' (60 sec by default). If `delay = -1`, the application is **not** closed, only
#' the session is closed when the user clicks on the quit button.
#' @param config The result of the call to [config()], if done.
#'
#' @return The code to be inserted in the server part of the learnitdown Shiny
#' application in order to properly identify the user and record the events.
#' @export
#'
#' @seealso [learnitdownShinyVersion()], [sign_in()]
trackEvents <- function(session, input, output, sign_in.fun = NULL,
url = Sys.getenv("MONGO_URL"), url.server = Sys.getenv("MONGO_URL_SERVER"),
db = Sys.getenv("MONGO_BASE"), user = Sys.getenv("MONGO_USER"),
password = Sys.getenv("MONGO_PASSWORD"),
version = getOption("learnitdown.shiny.version"),
path = Sys.getenv("LEARNITDOWN_LOCAL_STORAGE", "shiny_logs"),
log.errors = TRUE, log.outputs = FALSE, drop.dir = TRUE, config = NULL,
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {

  # Indicate this is a learnitdown Shiny application
  debug <- isTRUE(debug)
  if (debug)
    message("Shiny application with learnitdown v. ",
      packageVersion("learnitdown"))

  # Increment a session counter
  session_counter <- getOption("learnitdown.shiny.sessions", default = 0) + 1
  options(learnitdown.shiny.sessions = session_counter)
  message("Running sessions: ", session_counter)

  # Install callbacks on session close
  observe({
    # Decrement the number of opened sessions on session close
    onSessionEnded(function() {
      session_counter <-
        getOption("learnitdown.shiny.sessions", default = 0) - 1
      options(learnitdown.shiny.sessions = session_counter)
      message("Running sessions: ", session_counter)
    })

    # Get user information
    user_info <- parseQueryString(session$clientData$url_search)
    # No login info? Check local fingerprint instead (Shiny app run locally?)
    if (is.null(user_info$login) & !is.null(sign_in.fun)) {
      message("Getting user information from the fingerprint.")
      user_info <- sign_in.fun()
    }

    # If there is still no login in user_info, we don't track events
    if (is.null(user_info$login)) {
      message("No login: no events will be tracked")
      toastr_warning("Utilisateur anonyme, aucun enregistrement.",
        closeButton = TRUE, position = "top-right", timeOut = 5000)
    } else {
      # Check that 'path' exists and is writeable, or use a temporary directory
      if (!dir.exists(path))
        dir.create(path, showWarnings = FALSE, recursive = TRUE)
      test_file <- file.path(path, "test.txt")
      res <- try(cat("test, can be deleted", file = test_file), silent = TRUE)
      # Switch to a temporary directory, if it is not there, or not writeable
      if (inherits(res, "try-error") || !file.exists(test_file))
        path <- file.path(tempdir(check = TRUE), session$token)
      unlink(test_file)

      # Check results of config, if provided
      if (!is.null(config) && !isTRUE(config)) {
        error.message <- paste("Problem during configuration!",
          as.character(attr(config, "error")))
        message(error.message)
        toastr_error(error.message, closeButton = TRUE,
          position = "top-right", timeOut = 60000)
      }
      message("Tracking events in ", path, " for user ", user_info$login)
      toastr_info(paste0("Enregistrement actif pour ", user_info$login),
        closeButton = TRUE, position = "top-right", timeOut = 5000)
      updateActionButton(session, "learnitdown_quit_", label = "Save & Quit")

      user_tracking <- function(session, query = user_info) {
        # This is the original shinylogs function to retrieve the user
        get_user <- function(session) {
          if (!is.null(session$user))
            return(session$user)
          shiny_user <- Sys.getenv("SHINYPROXY_USERNAME")
          if (shiny_user != "") {
            return(shiny_user)
          } else {
            getOption("shinylogs.default_user", default = Sys.info()[['user']])
          }
        }
        shiny_user <- get_user(session)

        if (!length(query)) {
          query <- list(user = shiny_user)
        } else {
          query$user <- shiny_user
        }
        query$pathname <- session$clientData$url_pathname
        as.character(toJSON(query, auto_unbox = TRUE))
      }

      track_usage(storage_mode = store_rds(path = path),
        get_user = user_tracking)

      onSessionEnded(function() {
        # Read log events from .rds files and insert them in a MongoDB database
        # If MONGO_URL_SERVER exists and we are run from a server, we use it,
        # otherwise, we use MONGO_URL
        is_local <- function()
          Sys.getenv('SHINY_PORT') == ""

        is_server_up <- function(url, db) {
          res <- try(mongo(collection = "events", db = db, url = url),
            silent = TRUE)
          !inherits(res, "try-error")
        }

        if (is_local()) {
          if (isTRUE(debug))
            message("Application runs locally")
          # We use url
        } else {
          if (isTRUE(debug))
            message("Application runs from a server")
          if (url.server != "" && is_server_up(url = glue(url.server), db = db))
            url <- url.server # Use server URL instead
        }
        if (isTRUE(debug))
          message("Database URL: ", url, ", base: ", db)
        res <- record_shiny(path, url = glue(url), db = db,
          version = version, log.errors = log.errors, log.outputs = log.outputs,
          drop.dir = drop.dir, debug = debug)

        if (!res) {# An error occurred!
          # This does not work because the UI is already closed!
          # So, we output a message instead

          message.error <- paste("Error while recording data: ",
            as.character(attr(res, "error")))
          ##showToast("error", message.error, .options = myToastOptions)
          #toastr_error(message.error, closeButton = TRUE)
          message(message.error)
        }
      })
    }
  })
}

.shiny_feedback <- function(success = TRUE, message.success, message.error,
  position = "bottom-center") {
  #myToastOptions <- list(
  #  positionClass = "toast-bottom-center",
  #  progressBar = FALSE,
  #  closeButton = TRUE)

  if (isTRUE(success)) {
    #showToast("success", message.success, .options = myToastOptions)
    toastr_success(message.success, closeButton = TRUE, position = position,
      timeOut = 5000)
  } else {
    #showToast("error", message.error, .options = myToastOptions)
    toastr_error(message.error, closeButton = TRUE, position = position,
      timeOut = 5000)
  }
}

#' @export
#' @rdname trackEvents
trackSubmit <- function(session, input, output, solution = NULL,
max_score = NULL, comment = "",
message.success = "Correct", message.error = "Incorrect", score.txt = "Score",
check.solution = check_shiny_solution) {
  if (!is.null(max_score))
    max_score <- as.numeric(max_score[1]) # Make sure max_score is a number

  observeEvent(input$learnitdown_submit_, {
    req(input$learnitdown_submit_)

    if (is.null(solution) || !length(names(solution))) {
      # Always TRUE
      answer <- NULL
      res <- TRUE
      if (!is.null(max_score)) {
        score <- max_score
      } else {
        score <- 1L
      }
      max <- score
    } else {
      # For each item, check if answer is correct (equal, or within range)
      items <- names(solution)
      answer <- list()
      for (item in items)
        answer[[item]] <- isolate(input[[item]])
      res <- check.solution(answer, solution)
      max <- length(res) # Default max score
      score <- sum(res)
      # Do we need to rescale score (max_score provided) ?
      if (!is.null(max_score)) {
        score <- score / max * max_score
        max <- max_score
      }
    }
    .shiny_feedback(all(res),
      message.success = message.success,
      message.error = paste0(message.error, "\n ", score.txt, " = ",
        round(score, 2), "/", max))
    val <- list(
      correct = all(res),
      score = score,
      max = max,
      answer = answer,
      solution = solution,
      match = as.list(res),
      comment = comment
    )
    val_str <- as.character(toJSON(val, auto_unbox = TRUE))
    updateTextInput(session, "learnitdown_result_", value = val_str)
  })
}

#' @export
#' @rdname trackEvents
trackQuit <- function(session, input, output, delay = 60) {
  observeEvent(input$learnitdown_quit_, {
    req(input$learnitdown_quit_)
    session$close()
    # Force closing the app after delay if there is no other opened session
    if (delay != -1)
      later::later(function() {
        if (getOption("learnitdown.shiny.sessions", default = 2) < 1)
          stopApp()
       }, delay = delay)
  })
}

#' @export
#' @rdname trackEvents
check_shiny_solution <- function(answer, solution) {
  # For each item, check if answer is correct (equal, or within range)
  items <- names(solution)
  res <- logical(0)
  for (item in items) {
    sol_item <- solution[[item]]
    if (is.character(sol_item)) { # Characters strings, is answer in solution?
      res[item] <- all(answer[[item]] %in% sol_item)
    } else {
      # Treat it as numeric (even if it is logical)
      # If there are two numbers and they are named min & max, use them as
      # a range where values should be comprised
      if (length(sol_item) == 2 && !is.null(names(sol_item)) &&
          all(names(sol_item) == c("min", "max"))) {
        res[item] <- min(answer[[item]]) >= sol_item["min"] &
          max(answer[[item]]) <= sol_item["max"]
      } else if (length(answer[[item]]) > 1) { # For sliders,
        # check each value individually
        res[item] <- all(answer[[item]] == sol_item)
      } else {# Only one value returned, check in set
        res[item] <- any(answer[[item]] == sol_item)
      }
    }
  }
  res
}
