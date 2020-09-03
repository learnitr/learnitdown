#' Launch Shiny application by clicking on its screenshot.
#'
#' Shiny applications can be embedded in certain R Markdown documents. However,
#' the application is automatically loaded at the same time as the main page,
#' and that may not be the desired behavior. With [launch_shiny()], you display
#' just a screenshot of the Shiny application in the page. The user has to click
#' on it to actually launch the application (that replaces the screenshot on the
#' page).
#'
#' @param url The URL of the Shiny application. If both `app =` and `baseurl =`
#' are provided, you don't need to specify it.
#' @param app Name of the shiny application (cannot be duplicated on a page).
#' @param imgdir The directory without trailing "/" where images relative
#' to Shiny applications are stored. By default, it is relative to current
#' directory, in `images/shinyapps` subdirectories.
#' @param img The relative or absolute path to the image with a screenshot of
#' the Shiny application, as produced by [webshot_shiny()].
#' @param createimg If the app image (`img`) is not found, and there is no
#' default image in `imgdir =`, do we put it there (yes be default)?
#' @param width The width of the image and iframe for the app.
#' @param height The height of image and iframe.
#' @param fun The function to run as alternative to start the Shiny application
#' locally. It is better to fully specify it (`package::function`), and it
#' should take one argument which is the application name in `app =`.
#' @param alt1 Alternate text to display at the bottom of the screenshot. If
#' `NULL`, nothing is displayed below the screenshot.
#' @param alt2 Alternate text to display at the bottom of the screenshot in case
#' `fun =` is provided.
#' @param ... Not used here, but it allows to add more arguments used by the
#' screenshot addin, like `delay =`, `offsetx =` or `offsety =`, see
#' [webshot_shiny()].
#'
#' @return The HTML content that creates the image and the iframe. The function
#' must be called from within an R inline expression or from an R chunk with
#' `results='asis'` in an HTML-rendered version of the R Markdown document to
#' get the correct result.
#'
#' @export
#'
#' @seealso [webshot_shiny()]
#' @examples
#' # TODO...
launch_shiny <- function(url, app = basename(url),
imgdir = "images/shinyapps", img = paste0(imgdir, "/", app, ".png"),
createimg = TRUE, width = 780, height = 500, fun,
alt1 = "*Click to start the Shiny application.*",
alt2 = "*Click to start or run `{fun}(\"{app}\")`.*", ...) {
  # Default value for app
  if (missing(app))
    app <- basename(url)

  # Use alt2 if fun is provided, otherwise, use alt1
  if (!missing(fun) && !is.null(fun) && fun != "") {
    alt <- glue(alt2)
  } else {
    alt <- glue(alt1)
  }
  if (!length(alt))
    alt <- NULL

  # Check if the img exists, or fall back to a default image instead
  if (!file.exists(img)) {
    # If the default image does not exists, copy it from the package now
    img <- paste0(imgdir, "/shinyapp_default.png")
    if (!file.exists(img) & isTRUE(createimg)) {
      # Make sure imgdir directory exists
      dir.create(imgdir, showWarnings = FALSE, recursive = TRUE)
      # Copy the default image from the learndown package
      file.copy(from = system.file("images", "shinyapp_default.png",
        package = "learndown"), to = img)
    }
  }

  # If width and height are not explicitly provided, guess them from img size
  if (missing(width) || missing(height)) {
    info <- image_info(image_read(img))
    if (missing(width) && !is.null(info$width))
      width <- info$width
    if (missing(height) && !is.null(info$height))
      height <- info$height
  }

  # Construct the HTML code to be included
  res <- glue("\n<img onclick=\"launchApp('{app}', '{url}');\" src=\"{img}\" width=\"{width}\" height=\"{height}\" class=\"shiny-img\" id=\"img{app}\"/>
<iframe width=\"{width}\" height=\"{height}\" frameborder=\"0\" scrolling=\"auto\" style=\"display:none\" class=\"shiny-app\" id=\"{app}\"></iframe>\n")
  if (!is.null(alt))
    res <- paste0(res, "\n", alt, "\n")
  res
}
