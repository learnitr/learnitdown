# learndown 1.1.1

- Anchors now include `part` for `assignation()`, so that different anchors are
used if different parts of the same assignation are proposed in the same
section/module of the bookdown.

- Anchor now includes both the image and the iframe for `launch_shiny()`.

- An error message is now displayed in Shiny applications and learnr tutorials
if the database is not accessible.

- In learnr tutorials, access to database is tested only once. If it fails, no
further attempt is done in this process in order to avoid too much slow down in
systematically trying to connect to a non responding database for each event.

# learndown 1.1.0

- Addition of `learnr()` and `assignation()` functions to create tutorial and
GitHub assignation blocks. They use two new blocks: `tuto` and `assign` blocks
with specific icons.

- Addition of `toc` arguments in `h5p()`, `launch_shiny()`, `'learnr()` and
`assignation()` and a function to generate the exercises table of content
with `show_ex_toc()`. Anchors are added in the respective items, so that the
toc can link to them. The four types of items are also associated with specific
icons that link to help pages when the user clicks them.

- Links to start Shiny applications and learnr tutorials directly in RStudio
server in the SciViews Box are added.

# learndown 1.0.7

- The solution verification in `trackSubmit()` is now customizable, and a much
more complete default is provided in `check_shiny_solution()`.
- Default `delay =` for `webshot_shiny()` now at 10 sec in order to catch more
correct case with default value.
- The `run()` function now runs a tutorial in the RStudio tab if possible.

# learndown 1.0.6

- The url_pathname is now integrated to the user information in Shiny
applications, and it is used to get a more meaningful app name in the case of
RStudio Server (otherwise, it is always 'app').
- `fingerprint()` is renames `sign_in()`: more explicit.

# learndown 1.0.5

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
- New argument `fingerprint.fun =` in `trackEvents()` so that locally run Shiny
applications can also use user data stored in the fingerprint cache.
- `learndownLearnrSetup()` to initialize easily learndown learnr apps.

# learndown 1.0.4

- Functions for learndown learnr applications added.
- `config()` function added.
- Reworked database scheme, including course and institution
- Use of `LEARNDOWN_LOCAL_STORAGE` and `LEARNDOWN_DEBUG` environment variables
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
