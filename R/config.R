#' Configure the R environment for the course (including database information)
#' and provide (or cache) user information in ciphered form.
#'
#' Call these functions every time you need to get environment variables set,
#' like the URL, user and password of the MongoDB database used by the course.
#' @param url The URL of the encrypted file that contains the configuration
#' information.
#' @param data The fingerprint data in clear or ciphered form (in this case, the
#' string must start with "fingerprint=").
#' @param password The password to crypt, decrypt, lock or unlock the data.
#' @param cache The path to the file to use to store a cached version of these
#' data. Access to the database will be checked, and if it fails, the
#' configuration data are refreshed from the URL.
#' @param debug Do we issue debugging messages? By default, it is set according
#' to the `LEARNITDOWN_DEBUG` environment variable (yes, if this variable is not
#' `0`).
#' @param object An object to be encrypted, decrypted, locked or unlocked.
#' @param cipher The cryptography algorithm to use.
#' @param iv The initialization vector for the cipher.
#' @param serialize Do we serialize `object` before ciphering it (`TRUE` by
#' default)?
#' @param unserialize Do we unserialize the resulting object after deciphering
#' it (`TRUE` by default)?
#' @param base64 Do we encode/decode base64 the object (`FALSE` by default)?
#' @param url.encode Do we encode for URL (query) use (`FALSE` by default) ?
#' @param url.decode Do we decode URL before deciphering it (`FALSE` by
#' default)?
#' @param title The title of the dialog box prompting to sign out.
#' @param message The message of the dialog box prompting to sign out, or asking
#' for a password.
#' @param key The key that stores the password. It is advised to use something
#' like `course_year`, so that you can manage different passwords for the same
#' course given at different academic years. Using a key, the course password
#' must be entered only once. If not provided, the password is not stored. Note
#' also that the name of an environment variable could be used too for the key.
#' This is convenient on a server like RStudio Connect, for instance.
#' @param reset Should we reset the password (`FALSE` by default)?
#' @param ref1 Code to check the validity of the password
#' @param ref2 Second code to check password validity.
#'
#' @return Invisibly returns `TRUE` if success, or `FALSE` otherwise for
#' [config()]. The encrypted/decrypted object for [encrypt()] and [decrypt()],
#' or the locked/unlocked object for [lock()] and [unlock()].
#' The user information for [sign_in()].
#' @export
config <- function(url, password,
cache = file.path(tempdir(), ".learnitdown_config"),
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {
  debug <- isTRUE(debug)
  # Make sure the environment variable is set correctly for debug
  Sys.setenv(LEARNITDOWN_DEBUG = as.integer(debug))

  # Set environment variables according to entries in an encrypted configuration
  # file, and return the encrypted data, if it succeeds (test database access)
  setenv <- function(file, password, debug) {
    try({
      conf_crypt <- readRDS(file)
      conf <- decrypt(conf_crypt, password = password)
      # Set environment variables
      for (item in names(conf)) {
        if (Sys.getenv(item) != "")
          conf[[item]] <- NULL # Do not replace this item if already there
      }
      if (length(conf))
        do.call(Sys.setenv, conf)

      # Test the access to the MongoDB database, using env. vars
      user <- Sys.getenv("MONGO_USER")
      password <- Sys.getenv("MONGO_PASSWORD")
      mongo_url <- Sys.getenv("MONGO_URL")
      db <- Sys.getenv("MONGO_BASE")
      if (debug)
        message("Database URL: ", mongo_url, ", base: ", db)
      mongo(collection = "learnr", db = db, url = glue(mongo_url))
      # Return conf_crypt
      conf_crypt
    }, silent = TRUE)
  }

  # If the cache file is there, use it
  if (file.exists(cache)) {
    res <- setenv(cache, password = password, debug = debug)
    if (!inherits(res, "try-error")) {
      if (debug) {
        message("Learnitdown configuration set from cache")
        message("Database is responding")
      }
      return(invisible(TRUE))
    }
  }

  # If no file cache, or an error occurs, try getting the config file from url
  # First check if Internet connexion is alive
  check_internet_access <- function() {
    # IANA's test website
    is_online <- function(site="http://example.com/") {
      tryCatch({
        readLines(site, n = 1)
        TRUE
      },
        warning = function(w) invokeRestart("muffleWarning"),
        error = function(e) FALSE)
    }
    # We test two different sites successively
    res <- is_online("http://www.google.com")
    if (!res)
      res <- is_online("http://www.github.com")
    if (!res)
      stop("This computer does not seems to have access to the Internet, ", "
        impossible to record events in the database ",
        "(but they are saved on this computer for now).", call. = FALSE)
  }

  res <- try(check_internet_access(), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (debug)
      message("No access to the Internet: ", res)
    return(invisible(structure(FALSE, error = res)))
  }
  res <- setenv(url(url), password = password, debug = debug)
  if (inherits(res, "try-error")) {
    if (debug)
      message("Inaccessible or incorrect configuration or ",
        "database not responding: ", res)
    return(invisible(structure(FALSE, error = res)))
  } else {
    if (debug) {
      message("Learnitdown configuration set from URL")
      message("Database is responding")
    }
    # Save these data into the cache file
    try(suppressWarnings(saveRDS(res, file = cache)), silent = TRUE)
    return(invisible(TRUE))
  }
}

#' @rdname config
#' @export
sign_in <- function(data, password, cipher = "aes-256-cbc", iv = NULL,
cache = file.path(tempdir(), ".learnitdown_user"),
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {
  debug <- isTRUE(debug)
  if (!missing(data)) {
    # If data = NULL, we delete fingerprint data (that is, user is signed out).
    if (is.null(data)) {
      unlink(cache, force = TRUE)
      if (debug)
        message("User information deleted (fingerprint cache).")
      return(invisible(NULL))
    }
    # We set or replace fingerprint cache file
    data <- as.character(data)
    # If data are already encrypted, the string starts with "fingerprint="
    if (substring(data, 1, 12) != "fingerprint=") {
      # We encrypt these data
      data <- encrypt(data, password = password, cipher = cipher, iv = iv,
        serialize = FALSE, base64 = TRUE, url.encode = TRUE)
      data <- paste0("fingerprint=", data)
    }
    writeBin(data, cache)
    if (debug)
      message("Fingerprint data cached.")
  } else {
    if (file.exists(cache)) {
      data <- readBin(cache, "character")
      if (debug)
        message("Fingerprint retrieved from cache.")
    } else {
      # No data... nothing to do!
      if (debug)
        message("No fingerprint data.")
      return(invisible(NULL))
    }
  }

  # If password is provided, decrypt data
  if (!missing(password)) {
    data <- sub("^fingerprint=", "", data)
    user_info_query <- try(decrypt(data, password = password, cipher = cipher,
      iv = iv, unserialize = FALSE, base64 = TRUE, url.decode = TRUE),
      silent = TRUE)
    if (inherits(user_info_query, "try-error")) {
      if (debug)
        message("Error deciphering fingerprint: ", user_info_query)
      return(invisible(NULL))
    }
    # Parse this query string
   user_info <- parse_url(paste0("http://localhost?", user_info_query))$query
   # Check: we must have a list with at least a login component
   if (!is.list(user_info)) {
     if (debug)
       message("Expected a list for fingerprint data, got ", class(user_info))
     return(invisible(NULL))
   }
   if (!"login" %in% names(user_info)) {
     if (debug)
       message("No 'login' in fingerprint user data")
     return(invisible(NULL))
   }
   # Data should be OK
   if (debug)
     message("Successfully got user data from the fingerprint")
   return(invisible(user_info))
  } else {
    return(invisible(data))
  }
}

#' @rdname config
#' @export
sign_out <- function(title = "Signing out",
message = "Do you really want to sign out with learnitdown?",
cache = file.path(tempdir(), ".learnitdown_user"),
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {
  debug <- isTRUE(debug)

  if (rstudioapi::isAvailable()) {
    res <- rstudioapi::showQuestion(title, message)
  } else {
    res <- isTRUE(askYesNo(message))
  }
  if (res) {
    unlink(cache, force = TRUE)
    if (debug)
    message("Signed out!")
  }
  res
}

serialized <- NULL # To avoid a spurious warning in R CMD check

#' @rdname config
#' @export
encrypt <- function(object, password, cipher = "aes-256-cbc", iv = NULL,
serialize = TRUE, base64 = FALSE, url.encode = FALSE) {
  password <- as.character(password)
  if (length(password) != 1)
    stop("Use a single character string for the password")
  if (!nchar(password))
    stop("The password cannot be empty")

  # Create a secure key from the password
  key <- PKI.digest(charToRaw(password), "SHA256")


  if (isTRUE(serialize)) {
    serialize_con <- textConnection("serialized", "w", local = TRUE)
    dput(object, file = serialize_con, control = "all")
    close(serialize_con)
    object <- charToRaw(paste(serialized, collapse = "\n"))
  } else {
    object <- charToRaw(as.character(object))
  }

  # Encrypt object
  encrypted <- PKI.encrypt(object, key, cipher = cipher, iv = iv)

  if (isTRUE(base64))
    encrypted <- gsub("\n", "", base64_enc(encrypted))

  if (isTRUE(url.encode))
    encrypted <- URLencode(encrypted, reserved = TRUE)

  encrypted
}

#' @rdname config
#' @export
decrypt <- function(object, password, cipher = "aes-256-cbc", iv = NULL,
  unserialize = TRUE, base64 = FALSE, url.decode = FALSE) {
  password <- as.character(password)
  if (length(password) != 1)
    stop("Use a single character string for the password")
  if (!nchar(password))
    stop("The password cannot be empty")

  # Create a secure key from the password
  key <- PKI.digest(charToRaw(password), "SHA256")

  # Decode URL
  if (isTRUE(url.decode))
    object <- URLdecode(object)

  # Decode base 64
  if (isTRUE(base64))
    object <- base64_dec(object)

  # Decrypt
  decrypted <- PKI.decrypt(object, key, cipher = cipher, iv = iv)
  decrypted <- rawToChar(decrypted)

  # Possibly deserialize the object
  if (isTRUE(unserialize)) {
    decrypted <- dget(textConnection(decrypted))
  }

  decrypted
}

#' @rdname config
#' @export
lock <- function(object, password, key = "",
message = "Password for learnitdown:", reset = FALSE,
ref1 = NULL, ref2 = NULL) {
  if (missing(password))
    password <- .get_password(key, message = message, reset = reset,
      ref1 = ref1, ref2 = ref2)
  if (!nchar(password))
    stop("Password unknown or not provided, cannot lock object")

  encrypt(object, password = password, base64 = TRUE)
}

#' @rdname config
#' @export
unlock <- function(object, password, key = "",
message = "Password for learnitdown:", reset = FALSE,
ref1 = NULL, ref2 = NULL) {
  if (missing(password))
    password <- .get_password(key, message = message, reset = reset,
      ref1 = ref1, ref2 = ref2)
  if (!nchar(password))
    stop("Password unknown or not provided, cannot unlock object")

  decrypt(object, password = password, base64 = TRUE)
}

# Get the password for lock()/unlock()
.get_password <- function(key, message = "Password for learnitdown:",
  store = TRUE, reset = FALSE, ref1 = NULL, ref2 = NULL) {
  # First try to get it from environment variables (e.g., RStudio Connect)
  if (nchar(key)) {
    pass <- Sys.getenv(key)
    token <- Sys.getenv("SATURN_TOKEN", unset = "")
    if (nchar(token))
      pass <- decrypt(pass, token, base64 = TRUE, url.decode = TRUE)
  } else {
    pass <- ""
  }
  if (!is.null(pass) && length(pass) && nchar(pass))
    return(pass)

  # Otherwise, try getting it from keyring
  if (!isTRUE(reset) && nchar(key)) {
    pass <- try(key_get(service = "org.sciviews.learnitdown",
      username = key), silent = TRUE)
    if (!inherits(pass, "try-error"))
      return(pass)
  }

  # Ultimately, ask for it... (only in interactive mode)
  if (interactive())
    pass <- getPass(message)
  if (is.null(pass))
    pass <- ""
  # If the password is not blank and ref1/ref2 provided, check it now
  if (!is.null(pass) && length(pass) && nchar(pass) &&
    !is.null(ref1) && length(ref1) && nchar(ref1)) {
    if (!is.null(ref2)) {
      res <- try(unlock(ref1, ref2) != pass, silent = TRUE)
      if (inherits(res, "try-error"))
        stop("Error while checking if password is correct!")
      if (res)
        stop("Incorrect password!")
    } else {
      res <- try(unlock(ref1, pass) != "correct", silent = TRUE)
      if (inherits(res, "try-error"))
        stop("Error while checking if password is correct!")
      if (res)
        stop("Incorrect password!")
    }
  }

  # If the password and key are not blank, store it in keyring
  if (nchar(pass) && nchar(key)) {
    res <- try(key_set_with_value(service = "org.sciviews.learnitdown",
      username = key, password = pass), silent = TRUE) # We prefer no error
    # message in case it does not work (then we just will have to provide the
    # password every time we need it)
    if (inherits(res, "try-error")) {
      warning("The password cannot be stored in the keyring backend")
      attr(pass, "error") <- res
    }
  }
  pass
}
