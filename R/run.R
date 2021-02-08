#' Run learndown learnr tutorials or Shiny apps from a package after update
#'
#' These functions are convenient in the framework of a course whose learnrs and
#' Shiny applications may be updated during the course. The [update()] function
#' checks if an update is available (respecting the version of R), and the
#' [run()] (for learnrs), or [runApp()] (for Shiny applications) manage to
#' run the item in a friendly way.
#'
#' @param tutorial The name of the tutorial to use. If not provided, a list of
#' available tutorials is displayed.
#' @param app The name of the Shiny application to run. If not provided, a list
#' of available apps is displayed.
#' @param package The package from where to run the tutorial.
#' @param github_repos The GitHub repository where the package is developed (for
#' updates), use `NULL` to prevent any updates.
#' @param ... Further arguments passed to [run_tutorial()] (for learnrs), or to
#' [runApp()] (for Shiny applications).
#' @param update Do we check for an updated version first, and if it is found,
#' update the package automatically?
#' @param ask In case `tutorial` or `app` is not provided, do we ask to select
#' in a list?
#' @param upgrade When a new version of the main package is found, do we also upgrade
#' dependencies ? By default, never, but use `"ask"` to ask user.
#' @param in.job Should the application be run in a Job in RStudio (`TRUE` by
#' default)?
#'
#' @return The result returned by [run_tutorial()] for [run()], or by
#' [runApp()] for [run_app()]. The [update()] function return `TRUE` or
#' `FALSE`, depending if the package is updated or not.
#' @export
#' @seealso [run_tutorial()], [runApp()]
#' @keywords utilities
#' @concept run interactive learnr documents from the BioDataScience package
#' @examples
#' \dontrun{
#' #' # To start from a list of available tutorials:
#' run(package = "my_package")
#' run("my_tutorial", package = "my_package")
#' run_app(package = "mypackage")
#' run_app("my_shiny_app", package = "mypackage")
#' }
run <- function(tutorial, package, github_repos = NULL, ..., update = ask,
  ask = interactive(), upgrade = "never") {

  if (isTRUE(update) && !is.null(github_repos))
    updated <- update_pkg(package, github_repos, upgrade = upgrade)

  if (missing(tutorial) || is.null(tutorial) || tutorial == "") {
    tutos <- dir(system.file("tutorials", package = package))
    if (isTRUE(ask) && interactive()) {
      # Allow selecting from the list...
      sel <- select.list(tutos, title = "Select a tutorial")
      if (sel != "") {
        return(run(sel, package = package, github_repos = github_repos, ...,
          update = FALSE, ask = FALSE))
      } else return()
    } else {
      return(tutos)
    }
  }
  run_tutorial_tab <- get0(".rs.tutorial.runTutorial", envir = .GlobalEnv,
    mode = "function", inherits = TRUE)
  if (!is.null(run_tutorial_tab)) {
    # Run the tutorial in the Rstudio Tutorial tab
    run_tutorial_tab(tutorial, package = package, ...)
    # Make sure the tutorial is displayed in the Tutorial tab in RStudio
      registry <- get0(".rs.tutorial.registry", envir = .GlobalEnv,
        mode = "function", inherits = TRUE)
      if (!is.null(registry)) {
        reg_tutorial <- registry[[paste(package, tutorial, sep = "::")]]
        url <- reg_tutorial$shiny_url
        scalar_lfl <- get0(".rs.scalarListFromList", envir = .GlobalEnv,
          mode = "function", inherits = TRUE)
        if (!is.null(scalar_lfl)) {
          meta <- scalar_lfl(reg_tutorial)
          launch <- get0(".rs.invokeShinyTutorialViewer", envir = .GlobalEnv,
            mode = "function", inherits = TRUE)
          if (!is.null(url) && !is.null(launch))
            launch(url, meta)
        }
      }
  } else {
    # This is the classical learnr function, but the tutorial does not run in
    # the tutorial tab of RStudio in this case!
    run_tutorial(tutorial, package = package, ...)
  }
}

#' @rdname run
#' @export
run_app <- function(app, package, github_repos = NULL, ..., update = ask,
  ask = interactive(), upgrade = "never", in.job = TRUE) {

  if (isTRUE(update) && !is.null(github_repos))
    updated <- update_pkg(package, github_repos, upgrade = upgrade)

  if (missing(app) || is.null(app) || app == "") {
    apps <- dir(system.file("shiny", package = package))
    if (isTRUE(ask) && interactive()) {
      # Allow selecting from the list...
      sel <- select.list(apps, title = "Select a Shiny application")
      if (sel != "") {
        return(run_app(sel, package = package, github_repos = github_repos,...,
          update = FALSE, ask = FALSE, in.job = in.job))
      } else return()
    } else {
      return(apps)
    }
  }
  appDir <- system.file("shiny", app, package = package)
  port <- httpuv::randomPort()

  # Should we run the app in a job in RStudio?
  if (!isTRUE(in.job) || !rstudioapi::isAvailable()) {
    shiny::runApp(appDir, port = port, launch.browser = rstudioapi::viewer,
      display.mode = "normal")
  } else {
    script <- tempfile(pattern = "shiny", fileext = ".R")
    cat("shiny::runApp('", appDir, "', port = ", port,
      ", launch.browser = TRUE, display.mode = 'normal')\n",
      #", launch.browser = rstudioapi::viewer, display.mode = 'normal')\n",
      file = script, sep = "")
    rstudioapi::jobRunScript(script, name = paste("Shiny:", app, sep = ' '))
    later::later(function() unlink(script), delay = 10)
  }
}

#' @rdname run
#' @export
update_pkg <- function(package, github_repos, upgrade = "never") {
  if (is.null(github_repos))
    return(FALSE)

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
      httr::authenticate(token, "x-oauth-basic", "basic")
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
    if (!grepl("^box[0-9]{4}", hostname)) {
      # Get equivalent year, based on R version
      major <- as.integer(R.version$major)
      minor <- as.integer(R.version$minor)
      if (major == 4) {
        box_year <- as.character(2021 + minor)
      } else if (major == 3) {
        box_year <- switch(as.character(minor),
          "6" = "2020",
          "5" = "2019",
          "2018")
      } else {# Assume older version
        box_year <- "2018" # The oldest version managed
      }
    } else {# Get the version for the svbox
      # Get the year of the SciViews Box
      box_year <- substr(hostname, 4, 7)
    }
    # Pattern is v[box_year].x.y
    v_pat <- paste0("^[vV]", box_year, "\\.[0-9]+\\.[0-9]+$")

    # Get all tags for the repository
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
  last_tag <- get_last_tag(github_repos)
  if (!is.null(last_tag)) {
    last_rel <- sub("^[vV]([0-9]+\\.[0-9]+)\\.([0-9]+)$", "\\1-\\2", last_tag)
    curr_rel <- sub("^([0-9]+\\.[0-9]+)\\.([0-9]+)$", "\\1-\\2",
      packageVersion(package))
    status <- try(compareVersion(last_rel, curr_rel), silent = TRUE)
    if (!inherits(status, "try-error")) {
      if (status > 0) {
        # We need to update the package
        message("Updating the '", package, "' package... please, be patient")
        install_github(paste0(github_repos, "@", last_tag), upgrade = upgrade)
        new_rel <- sub("^([0-9]+\\.[0-9]+)\\.([0-9]+)$", "\\1-\\2",
          packageVersion(package))
        try(updated <- compareVersion(new_rel, last_rel) == 0, silent = TRUE)
      } else {
        # OK, we are already updated
        updated <- TRUE
      }
    }
  }
  updated
}
