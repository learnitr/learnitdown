# RStudio addins


# Shiny Application Webshot -----------------------------------------------

webshot_shiny_addin <- function() {
  # Gets The active Document
  ctx <- rstudioapi::getActiveDocumentContext()

  # Checks that a document is active
  if (!is.null(ctx)) {

    # Extracts selection as a string
    sel <- ctx$selection[[1]]$text

    # Check that the selection contains someting like a call to a function
    if (!grepl("^[^\\(]+(\\([^\\)]+\\)).*$", sel)) {
      message("You must select a construct like 'launch_shiny(url, ...)' first")
      return()
    }

    # Get what looks like the arguments
    args <- sub("^[^\\(]+(\\([^\\)]+\\)).*$", "\\1", sel)

    # Construct the code to take screenshot of the Shiny app and run it
    message("Launching the Shiny application and taking the screenshot,",
      " please wait...")
    img <- NULL
    code <- paste0("img <- learnitdown::webshot_shiny", args)
    res <- try(eval(parse(text = code)), silent = TRUE)

    # Is there an error?
    if (inherits(res, "try-error")) {
      message("An error occured while trying to take Shiny app screenshot")
      message(res)
    } else if (is.null(img)) {
      # No screenshot was created
      message("The screenshot was not created, check your code!")
    } else {
      # Check result and display the image and a message to the R console
      message("Screenshot created in: ", img)
      print(magick::image_read(img))
    }
  }
}
