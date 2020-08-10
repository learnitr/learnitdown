#' Initialize learndown features in an R Markdown document
#'
#' This function must be called at the beginning of a R Markdown document (e.g,
#' bookdown) to install required CSS and Javascript.
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
#'
#' @return The content to be inserted in the HTML document with CSS and
#' Javascript code required to implement the various learndown features.
#' @export
#'
#' @examples
#' # This is better placed in a setup R chunk or an R inline expression on its
#' # own line. To see the code injected, use `cat()` at the R prompt:
#' cat(learndown_init())
learndown_init <- function(shiny = TRUE, h5p = TRUE, use.query = FALSE,
iframe.links = TRUE, details.css = TRUE,
institutions = c("institution1", "institution2"),
courses = c("course1", "course2", "course3")) {

  # Add styles for institution/noinstitution & course/nocourse
  if (!length(institutions)) institutions <- "institution" # At least one item
  institutions_css <- paste0(".", institutions, collapse = " ")
  institutions_array <- paste0("let institutions = [",
    paste0("'", institutions, "'", collapse = ", "), ", 'noinstitution'];")

  if (!length(courses)) courses <- "course" # At least one item
  courses_css <- paste0(".", courses, collapse = " ")
  courses_array <- paste0("let courses = [",
    paste0("'", courses, "'", collapse = ", "), ", 'nocourse'];")

  # Construct style and script string
  res <- paste0("<style>
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
    res <- paste0(res, "
summary {
  background:  #f5f5f5;
  border: 1px solid #ccc;
}
")

  # Close style
  res <- paste0(res, "</style>

")

  # Get parameters from either localStorage or URL and store them in variables
  if (isTRUE(use.query)) {
    res <- paste0(res, "<script>
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
")
  } else {
    res <- paste0(res, "<script>
function getParameterByName(name, url) {
  name = name.replace(/[\\[\\]]/g,\"\\\\$&\");
  // Try to get the value from local storage
  if (window.localStorage) {
    return localStorage.getItem(name);
  } else {
    return '';
  }
}
")
  }
  res <- paste0(res, "
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
  res <- paste0(res, "
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
    res <- paste0(res, "
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
    res <- paste0(res, "
window.onload = processParameters;
")
  }
  # Handle parameters
  # TODO...

  # finalize
  res <- paste0(res, "</script>

<noscript>Please enable JavaScript for learndown extra features.</a></noscript>
")

res
}
