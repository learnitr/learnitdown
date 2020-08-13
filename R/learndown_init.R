#' Initialize learndown features in an R Markdown document
#'
#' This function must be called in a script run by `before_chapter_script` entry
#' in `_bookdown.yml` to create required `style.css` and `header.html` files.
#'
#' @param shiny Do we use Shiny applications and do we want to pass parameters
#' and or launch the application on a click?
#' @param h5p Do we use H5P served from a Wordpress site in the same domain as
#' our R Markdown document?
#' @param use.query Do we collect user/course/institution data through the URL
#' query string (the part after the question mark in the URL).
#' @param iframe.links If our document is displayed in an iframe, external link
#' should better target their parent window. With this option, external links
#' with no defined target are automatically retargeted when the page loads.
#' @param details.css Do we want to enhance the `<details>` section with a
#' summary surrounded by a light gray box in order to better evidence it.
#' @param institutions The list of possible institutions that have specific
#' sections in this document.
#' @param courses The list of courses with specific sections in this document.
#' @param style The path to the 'style.css' file.
#' @param style0 The path to a file with additional content to add to the
#' 'style.css' file.
#' @param header The path to the 'header.html' file.
#' @param header0 The path to a file with additional content to add to
#' 'header.html'.
#'
#' @return A list with `css` and `html` components with the content that was
#' added to respective files is returned invisibly for debugging purposes '(the
#' function is mainly used for its side effect of creating `style.css` and
#' `header.html` files for the bookdown format).
#' @export
#'
#' @examples
#' # This is better placed in a setup R chunk or an R inline expression on its
#' # own line. To see the code injected, use `cat()` at the R prompt:
#' odir <- setwd(tempdir())
#' dir.create("temp")
#' setwd("temp")
#' # Create fake style0.css and header0.html files to see what happens
#' cat("\n/* Content from style0.css */\n", file = "style0.css")
#' cat("\n<!-- Content from header0.html -->\n", file = "header0.html")
#' # Create style.css and header.html files
#' (learndown_init())
#' cat(readLines('style.css'), sep = "\n")
#' cat(readLines('header.html'), sep = "\n")
#' setwd("..")
#' unlink("temp")
#' setwd(odir)
#' rm(odir)
#'
learndown_init <- function(shiny = TRUE, h5p = TRUE, use.query = FALSE,
iframe.links = TRUE, details.css = TRUE,
institutions = c("institution1", "institution2"),
courses = c("course1", "course2", "course3"),
style = "style.css", style0 = "style0.css",
header = "header.html", header0 = "header0.html") {

  # Process style.css
  # Add styles for institution/noinstitution & course/nocourse
  if (!length(institutions)) institutions <- "institution" # At least one item
  institutions_css <- paste(paste0(".", institutions), collapse = ", ")
  institutions_array <- paste0("let institutions = [",
    paste0("'", institutions, "'", collapse = ", "), ", 'noinstitution'];")

  if (!length(courses)) courses <- "course" # At least one item
  courses_css <- paste(paste0(".", courses), collapse = ", ")
  courses_array <- paste0("let courses = [",
    paste0("'", courses, "'", collapse = ", "), ", 'nocourse'];")

  # Construct style and script string
  res_css <- paste0("
", institutions_css , " {
  display: none;
}

.noinstitution {
  display: block;
}

", courses_css, " {
  display: none;
}

.nocourse {
  display: block;
}
")

  # Add enhanced details summary (light gray box)
  if (isTRUE(details.css))
    res_css <- paste0(res_css, "
summary {
  background:  #f5f5f5;
  border: 1px solid #ccc;
}
")
  # Possibly add content form style0
  if (file.exists(style0))
    res_css <- paste0(res_css, "\n", paste(readLines(style0), collapse = "\n"))

  # Close style
  #res_css <- paste0(res_css, "
#")

  # Write results to style file
  cat(res_css, file = style, sep = "\n")

  # Process header.html
  # Get parameters from either localStorage or URL and store them in variables
  if (isTRUE(use.query)) {
    res_header <- "<script>
function getParameterByName(name, url) {
  if (!url) url = window.location.href;
  name = name.replace(/[\\[\\]]/g, \"\\\\$&\");
  var regex = new RegExp(\"[?&]\" + name + \"(=([^&#]*)|&|#|$)\"),
  results = regex.exec(url);
  if (!results) {
    // Try to get the value from local storage
    if (window.localStorage) {
      return localStorage.getItem(name);
    } else {
      return '';
    }
  }
  var value = '';
  if (results[2]) {
    value = decodeURIComponent(results[2].replace(/\\+/g, \" \"));
    if (window.localStorage) {
      localStorage.setItem(name, value);
    }
    return value;
  }
}
"
  } else {
    res_header <- "<script>
function getParameterByName(name, url) {
  name = name.replace(/[\\[\\]]/g,\"\\\\$&\");
  // Try to get the value from local storage
  if (window.localStorage) {
    return localStorage.getItem(name);
  } else {
    return '';
  }
}
"
  }
  res_header <- paste0(res_header, "
// Collect parameters and store their values (as passed by Moodle/Wordpress)
var login       = getParameterByName('login');
var email       = getParameterByName('email');
var displayname = getParameterByName('displayname');
var firstname   = getParameterByName('firstname');
var lastname    = getParameterByName('lastname');
var iemail      = getParameterByName('iemail');
var iid         = getParameterByName('iid');
var ifirstname  = getParameterByName('ifirstname');
var ilastname   = getParameterByName('ilastname');
var institution = getParameterByName('institution');
var icourse     = getParameterByName('icourse');
var ictitle     = getParameterByName('ictitle');
var iurl        = getParameterByName('iurl');
var iref        = getParameterByName('iref');
")

  # Add Javascript for institution/noinstitution & course/nocourse toggle
  res_header <- paste0(res_header, "
", institutions_array, "\n", courses_array, "

function toggleDisplay(item, target) {
  var style = item == target ? 'block' : 'none';
  var elems = document.getElementsByClassName(item);
  for (i = 0; i < elems.length; i++) {
    var elem = elems[i];
    elem.style.display = style;
  }
}

function toggleInstitution(name) {
  // Disable all institutions except that one
  // Since they are already all hidden, just reenable it and hide noinstitution
  toggleDisplay('noinstitution', name);
  toggleDisplay(name, name);
}

function toggleCourse(name) {
  // Disable all courses except that one
  // Since they are already all hidden, just reenable it and hide nocourse
  toggleDisplay('nocourse', name);
  toggleDisplay(name, name);
}

function processParameters() {
  // Content related to an institution
  if (institution !== null) {
    toggleInstitution(institution);
  }
  // Content relative to a course
  if (icourse !== null) {
    toggleCourse(icourse);
  }
  // Process other parameters too here...
  // ...
}

")

  # Possibly retarget links
  if (isTRUE(iframe.links)) {
    res_header <- paste0(res_header, "
function retargetLinks() {
  // If displayed in an iframe, open external links into parent
  // Adapted from Yihui Xie blog
  var links = document.getElementsByTagName('a');
  for (var i = 0; i < links.length; i++) {
    if (/^(https?:)?\\/\\//.test(links[i].getAttribute('href')) &&
      links[i].target != null) {
      links[i].target = '_parent';
    }
  }
};

window.onload = function() {processParameters(); retargetLinks();};
")
  } else {
    res_header <- paste0(res_header, "
window.onload = processParameters;
")
  }
  # Handle parameters
  # TODO...

  # Pass URL arguments to iframe 'app' (embedded shiny apps) & 'h5p'
  if (isTRUE(shiny) || isTRUE(h5p))
    res_header <- paste0(res_header, "
function encodeQueryParam(name, first = false) {
  var value = localStorage.getItem(name);
  if (value === null || value == '') return '';
  var sep = first ? '' : '&';
  return sep + encodeURIComponent(name) + '=' + encodeURIComponent(value);
}

function encodeQueryString() {
  // We got data from localStorage. So, if no data, no query string!
  if (!window.localStorage) return('');
  query = encodeQueryParam('login', true);
  query += encodeQueryParam('email') + encodeQueryParam('displayname');
  query += encodeQueryParam('firstname') + encodeQueryParam('lastname');
  query += encodeQueryParam('iemail') + encodeQueryParam('iid');
  query += encodeQueryParam('ifirstname') + encodeQueryParam('ilastname');
  query += encodeQueryParam('institution');
  query += encodeQueryParam('icourse') + encodeQueryParam('ictitle');
  query += encodeQueryParam('iurl') + encodeQueryParam('iref');
  // Detect if we have the Sepia or Night theme
  // TODO: refine this because it is *always* detected
  //if (document.getElementsByClassName('color-theme-1')) {
  //  if (query == '') {
  //    query = 'theme=Sepia';
  //  } else {
  //    query += '&theme=Sepia';
  //  }
  //}
  //if (document.getElementsByClassName('color-theme-22')) {
  //  if (query == '') {
  //    query = 'theme=Night';
  //  } else {
  //    query += '&theme=Night';
  //  }
  //}
  return query;
}

//var params = window.location.toString().split('?')[1];
var params = encodeQueryString();

if (params !== undefined && params != '') {
  var apps = document.getElementsByClassName('app');
  for (i = 0; i < apps.length; i++) {
    var appitem = apps[i];
    appitem.src = appitem.src + '?' + params;
  }

  //var h5ps = document.getElementsByClassName('h5p');
  //for (i = 0; i < h5ps.length; i++) {
  //  var h5pitem = h5ps[i];
  //  h5pitem.src = h5pitem.src + '&' + params;
  //}
}
")

  # Launch the Shiny app on click with URL parameters + theme
  if (isTRUE(shiny))
    res_header <- paste0(res_header, "
launchApp = function(id, src) {
  //var params = window.location.toString().split('?')[1];
  var params = encodeQueryString();
  if (params !== undefined && params != '') {
    src = src + '?' + params;
  }
  var img = document.getElementById('img' + id);
  var app = document.getElementById(id);
  app.src = src;
  app.style.display='block';
  img.style.display='none';
}
")

  # finalize
  res_header <- paste0(res_header, "</script>
")

  # Possibly add content form header0
  if (file.exists(header0))
    res_header <- paste0(res_header, "\n",
      paste(readLines(header0), collapse = "\n"))

  # Write results to header.html file
  cat(res_header, file = header, sep = "\n")

  # Return invisibly a list with css and html components
  invisible(list(style = res_css, html = res_header))
}
