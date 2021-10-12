# learnitdown 1.4.2

- Better checking of internet access during system configuration.

# learnitdown 1.4.1

- While recording learnr events, the procedure to check if the computer is connected to the Internet sometimes failed without good reasons. Now, check the access to two sites that should be more reliable.

- For `run_app()`, a better code is now used to display the app running in a job inside the RStudio viewer pane. A new argument `max.wait =` indicates the number of seconds to wait for the Shiny app to start.

# learnitdown 1.4.0

-   For `launch_shiny()`, allow to include a search string in the url.

# learnitdown 1.3.3

-   Better checking of Internet availability in `config()` and `record_learnr()`.

# learnitdown 1.3.2

-   Dependency to keyring eliminated.

# learnitdown 1.3.1

-   Experimental learnr obfuscation functions.

# learnitdown 1.3.0

-   Package renamed {learnitdown} to avoid conflicts with <http://learndown.com>.

-   New functions `lock()`/`unlock()`, using an application (e.g., course) password to lock or unlock sensitive data like students' personal information, or database password.

-   New obfuscation functions `obfuscate()`, `._()` and `O()` to hide {learnr} answers in the original .Rmd file. The function `ans()` could be used to replace `learnr::answer()` for a cleaner obfuscation mechanism for quizzes.

# learndown 1.2.1

-   Bug correction: when update_pkg() encountered a more recent version of the package, it upgraded it anyway.

# learndown 1.2.0

-   There are now more GitHub assignation types : `assign2()` for group assignations and `challenge()`/`challenge2()` for assignations linked to challenges (individual or in groups).

-   Images related to exercise blocks and suggested `style0.css` and `preamble.tex`to be used for {learndown}-enabled {bookdown}s are also provided now.

# learndown 1.1.8

-   Argument `upgrade=` added to `run()`, `run_app()` and `update_pkg()`. By default, it is "never", which is a good value inside the SciViews Box, but "ask" may be more appropriate elsewhere.

# learndown 1.1.7

-   Again a problem with bookdown links not correctly retargeted when the bookdown is displayed in an iframe. Corrected.

# learndown 1.1.6

-   For `assignation()`, links now have `target = "_blank"` so that these links are now functional, even inside an iframe.

# learndown 1.1.5

-   `run_app()` now opens the Shiny application in the default browser (but it is still the Viewer pane in RStudio).

-   In `learndown_init()`, links with `target=` attributes were retargeted to `_parent`, but it is those without `target=` that had to be changed (corrected).

-   In `learn()` and `launch_shiny()`, the link now have `target="_blank"` meaning that RStudio is opened in a different tab.

# learndown 1.1.4

-   `run()` now tries harder to display a tutorial in the Tutorial tab in RStudio.

# learndown 1.1.3

-   Gracefully exit in `run()` and `run_app()` when nothing is selected in the list.

# learndown 1.1.2

-   `webshot_shiny()` refused to work when it sees the `toc =` argument. Solved.

# learndown 1.1.1

-   Anchors now include `part` for `assignation()`, so that different anchors are used if different parts of the same assignation are proposed in the same section/module of the bookdown.

-   Anchor now includes both the image and the iframe for `launch_shiny()`.

-   An error message is now displayed in Shiny applications and learnr tutorials if the database is not accessible.

-   In learnr tutorials, access to database is tested only once. If it fails, no further attempt is done in this process in order to avoid too much slow down in systematically trying to connect to a non responding database for each event.

-   Internet access and configuration checking in Shiny applications, with a message displayed in case of error.

# learndown 1.1.0

-   Addition of `learnr()` and `assignation()` functions to create tutorial and GitHub assignation blocks. They use two new blocks: `tuto` and `assign` blocks with specific icons.

-   Addition of `toc` arguments in `h5p()`, `launch_shiny()`, `'learnr()` and `assignation()` and a function to generate the exercises table of content with `show_ex_toc()`. Anchors are added in the respective items, so that the toc can link to them. The four types of items are also associated with specific icons that link to help pages when the user clicks them.

-   Links to start Shiny applications and learnr tutorials directly in RStudio server in the SciViews Box are added.

# learndown 1.0.7

-   The solution verification in `trackSubmit()` is now customizable, and a much more complete default is provided in `check_shiny_solution()`.
-   Default `delay =` for `webshot_shiny()` now at 10 sec in order to catch more correct case with default value.
-   The `run()` function now runs a tutorial in the RStudio tab if possible.

# learndown 1.0.6

-   The url_pathname is now integrated to the user information in Shiny applications, and it is used to get a more meaningful app name in the case of RStudio Server (otherwise, it is always 'app').
-   `fingerprint()` is renames `sign_in()`: more explicit.

# learndown 1.0.5

-   The fields score, max and grade are added in all events tables with grade = score/max. The solution for Shiny applications can now use a range for numeric values and a series of correct solutions for text. Also a `max_score =` argument added to `trackSubmit()` allows to redefine the maximum score for a Shiny application.
-   fields type and id added to match what is issued by H5P.
-   `encrypt()` and `decrypt()` are now more versatile and accept more arguments to fine-tune their work.
-   `fingerprint()` function added to identify users in local learnr and shiny applications.
-   New argument `fingerprint.fun =` in `trackEvents()` so that locally run Shiny applications can also use user data stored in the fingerprint cache.
-   `learndownLearnrSetup()` to initialize easily learndown learnr apps.

# learndown 1.0.4

-   Functions for learndown learnr applications added.
-   `config()` function added.
-   Reworked database scheme, including course and institution
-   Use of `LEARNDOWN_LOCAL_STORAGE` and `LEARNDOWN_DEBUG` environment variables everywhere (Shiny and learnr applications).
-   Fields to be added to the database are adapted to get the same ones between the different applications.
-   Events replaced by their equivalent xAPI verbs.

# learndown 1.0.3

-   Allow using an alternate MongoDB server if a Shiny application is run from a server (using MONGO_URL_SERVER environment variable).

# learndown 1.0.2

-   Debug messages possibles in `trackEvents()` to determine the causes of errors when recording Shiny apps events in a MongoDB database.

# learndown 1.0.1

-   Shiny apps: do not track events when no login and fallback to temporary directory if `path =` is not writable in `trackEvents()`.

-   The number of opened sessions is tracked in a Shiny app and `stopApp()` is only invoked if there is no other opened session.

-   For Shiny applications, `trackEvents()` only tracks events in a MongoDB database for logged users. A message appears briefly (5sec) to indicate the status of the recording when the session is started.

# learndown 1.0.0

-   First version of the learndown package. Including code developed for BioDataScience-Course (show/hide sections depending on the institution or course, hide details, including some code chunks, include H5P, Shiny and learnr applications with user identification and events logging, defer Shiny/learnr apps until the user clicks on it, and an RStudio addin to take screenshots of such apps easily).
