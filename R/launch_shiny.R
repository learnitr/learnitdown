#' Placeholder with screenshot to launch Shiny application on click.
#'
#' Shiny applications can be embedded in certain R Markdown documents. However,
#' the application is automatically loaded at the same time as the main page,
#' and that may not be the desired behavior. With [launch_shiny()], you display
#' just a screenshot of the Shiny application in the page. The user has to click
#' on it to actually launch the application (that replaces the screenshot on the
#' page).
#'
#' @param name Name of the shiny application (cannot be duplicated on a page).
#' @param url The URL of the Shiny application.
#' @param img The relative or absolute path to the image. Best effect is
#' obtained with a slightly darkened screenshot of the actual application saved
#' in a PNG image of exactly the same size as the iframe that embeds the Shiny
#' application. You can also add an icon on top of the image that suggests to
#' click on it to launch the application for an even better effect.
#' @param width The width of the image and iframe for the app.
#' @param height The height of image and iframe.
#' @param alt Alternate text to display at the bottom of the screenshot. If
#' `NULL`, nothing is displayed below the screenshot.
#'
#' @return The HTML content that creates the image and the iframe. The function
#' must be called from within an R inline expression of an R chunk with
#' `results='asis'` in an HTML-rendered version of the R Markdown document to
#' get the correct result.
#'
#' @export
#'
#' @examples
#' # TODO...
launch_shiny <- function(name, url, img, width = 780, height = 500,
  alt = "*Click to start the application.*") {
  res <- glue::glue("\n<img onclick=\"launchApp('{name}', '{url}');\" src=\"{img}\" width=\"{width}\" height=\"{height}\" class=\"shiny-img\" id=\"img{name}\"/>
<iframe width=\"{width}\" height=\"{height}\" frameborder=\"0\" scrolling=\"auto\" style=\"display:none\" class=\"shiny-app\" id=\"{name}\"></iframe>\n")
  if (!is.null(alt))
    res <- paste0(res, "\n", alt, "\n")
  res
}
