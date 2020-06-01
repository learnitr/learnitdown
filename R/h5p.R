#' Insert H5P content in the document
#'
#' @param id The ID of the H5P content in your Wordpress.
#' @param baseurl The first part of the URL for your domain, usually something like `https://my.site.com` **without** the trailing `/`.
#' @param width The width of the iframe where the H5P content is displayed.
#' @param height The height of the iframe.
#'
#' @return HTML code that generates the iframe. It is most conveniently used inside and R inline expression in your R Markdown document on its own line with one blank line above and bellow it.
#' @export
#'
#' @details
#' This function is designed to work inside a Wordpress site where the H5P plugin has been installed. You should also serve your bookdown pages as a subdirectory inside of the same Wordpress site to allow free communication between the parent (the bookdown page) and the child document in the iframe (the H5P content).
h5p <- function(id, baseurl, width = 780, height = 500) {
  glue::glue("\n<iframe src=\"{baseurl}/wp-admin/admin-ajax.php?action=h5p_embed&id={id}\" width=\"{width}\" height=\"{height}\" frameborder=\"0\" allowfullscreen=\"allowfullscreen\" class=\"h5p\"></iframe><script src=\"{baseurl}/wp-content/plugins/h5p/h5p-php-library/js/h5p-resizer.js\" charset=\"UTF-8\"></script>\n")
}
