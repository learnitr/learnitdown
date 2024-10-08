#' Create the screenshot image of a Shiny application with a click icon
#'
#' The [webshot_shiny()] function is designed to create the screenshot image of
#' a Shiny application, with an icon suggesting to click on it for launching
#' the application. The image created can then be used by [launch_shiny()] in
#' a bookdown to differ the start of Shiny application, while displaying useful
#' information to the user (how the Shiny application would look like if it was
#' started).
#'
#' @param url The URL to launch the Shiny app, or to a screenshot of the app in
#' PNG format.
#' @param app The name of the Shiny application. If `NULL`, the base name of the
#' URL without the extension is used.
#' are provided, you don't need to specify it.
#' @param imgdir The directory without trailing "/" where images relative
#' to Shiny applications are stored. By default, it is relative to current
#' directory, in `images/shinyapps` subdirectories.
#' @param img The path to the image that is created. Not needed if `app =` and
#' `imgdir =` are provided.
#' @param width The requested weight of the screenshot (it may differ if the
#' Shiny application defines other (limit) values.
#' @param height The requested height of the screenshot (idem).
#' @param offsetx The offset from left where to place the click icon in pixels.
#' @param offsety The offset to bottom where to place the click icon in pixels.
#' @param delay Time to wait (in sec) after the Shiny application has started
#' and before the screenshot is taken. If the screenshot does not contain the
#' complete application UI, try increase this value.
#' @param ... Further arguments passed to [launch_shiny()] but not used here.
#'
#' @return The path to the created image, invisibly.
#' @export
#'
#' @seealso [launch_shiny()]
#' @examples
#' \dontrun{
#' # We wait 10 sec to make sure it is loaded when the screenshot is taken
#' (webshot_shiny("https://phgrosjean.shinyapps.io/histogram/", delay = 10))
#'  # Now, look at this image. You can use it with launch_shiny()
#'}
webshot_shiny <- function(url, app = NULL,
imgdir = "images/shinyapps", img = paste0(imgdir, "/", app, ".png"),
width = 790, height = 500, offsetx = 30, offsety = 30, delay = 10, ...) {
  # Make sure imgdir directory exists
  dir.create(imgdir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(app))
    app <- sub("\\.[^\\.]+$", "", basename(url))

  # Temporary screenshot and click icon images
  img_app_file <- paste0(imgdir, "/", app, "_temp.png")
  img_click_file <- system.file("images", "shinyapp_click.png",
    package = "learnitdown")

  # If url points to a .png file, just copy that file into img_app_file
  if (endsWith(url, ".png")) {
    file.copy(url, img_app_file)
  } else {
    # Launch the Shiny app, wait delay and take screenshot
    webshot(url, delay = delay, vwidth = width, vheight = height,
      file = img_app_file)
  }

  # Combine both images
  img_app <- image_read(img_app_file)
  img_click <- image_read(img_click_file)

  # If img_app is larger than width and height, crop it
  if (!missing(width) || !missing(height)) {
    info <- image_info(img_app)
    if (missing(width))
      width <- info$width
    if (missing(height))
      height <- info$height
    img_app <- image_crop(img_app, paste(width, height, sep = "x"))
  }

  # Create composite image by superposing the click icon
  img_comp <- image_composite(img_app, img_click,
    offset = paste0("+", offsetx, "+", offsety), gravity = "southwest")
  image_write(img_comp, path = img, format = "png")
  # Temporary screenshot not needed anymore
  unlink(img_app_file)

  # Return the path to the created image invisibly
  invisible(img)
}
