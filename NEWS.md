# learndown 1.0.4

- The fields score, max and grade are added in all events tables with
grade = score/max. The solution for Shiny applications can now use a range for
numeric values and a series of correct solutions for text. Also a `max_score =`
argument added to `trackSubmit()` allows to redefine the maximum score for a
Shiny application.
- fields type and id added to match what is issued by H5P.
- `encrypt()` and `decrypt()` are now more versatile and accept more arguments
to fine-tune their work.
- `fingerprint()` function added to identify users in local learnr and shiny
applications.

# learndown 1.0.4

- Functions for learndown learnr applications added.
- `config()` function added.
- Reworked database scheme, including course and institution
- Use of LEARNDOWN_LOCAL_STORAGE and LEARNDOWN_DEBUG environment variables
everywhere (Shiny and learnr applications).
- Fields to be added to the database are adapted to get the same ones between
the different applications.
- Events replaced by their equivalent xAPI verbs.

# learndown 1.0.3

- Allow using an alternate MongoDB server if a Shiny application is run from a
server (using MONGO_URL_SERVER environment variable).

# learndown 1.0.2

- Debug messages possibles in `trackEvents()` to determine the causes of errors
when recording Shiny apps events in a MongoDB database.

# learndown 1.0.1

- Shiny apps: do not track events when no login and fallback to temporary
directory if `path =` is not writable in `trackEvents()`.

- The number of opened sessions is tracked in a Shiny app and `stopApp()` is
only invoked if there is no other opened session.

- For Shiny applications, `trackEvents()` only tracks events in a MongoDB
database for logged users. A message appears briefly (5sec) to indicate the
status of the recording when the session is started.

# learndown 1.0.0

- First version of the learndown package. Including code developed for
BioDataScience-Course (show/hide sections depending on the institution or
course, hide details, including some code chunks, include H5P, Shiny and learnr
applications with user identification and events logging, defer Shiny/learnr 
apps until the user clicks on it, and an RStudio addin to take screenshots of
such apps easily).
