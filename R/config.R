#' Configure the R environment for the course (including database information)
#' and provide (or cache) user information in ciphered form.
#'
#' Call this function every time you need to get environment variables set, like
#' the URL, user and password of the MongoDB database used by the course.
#' @param url The URL of the encrypted file that contains the configuration
#' information.
#' @param data The fingerprint data in clear or ciphered form (in this case, the
#' string must start with "fingerprint=").
#' @param password The password to decrypt the data.
#' @param cache The path to the file to use to store a cached version of these
#' data. Access to the database will be checked, and if it fails, the
#' configuration data are refreshed from the URL.
#' @param debug Do we issue debugging messages? By default, it is set according
#' to the `LEARNITDOWN_DEBUG` environment variable (yes, if this variable is not
#' `0`).
#' @param object An object to be encrypted.
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
#' @param message The message of the dialog box prompting to sign out.
#'
#' @return Invisibly returns `TRUE` if success, or `FALSE` otherwise for
#' [config()]. The encrypted/decrypted object for [encrypt()] and [decrypt()].
#' The user information for [sign_in()].
#' @export
config <- function(url, password,
cache = file.path(tempdir(), ".learnitdown_config"),
debug = Sys.getenv("LEARNITDOWN_DEBUG", 0) != 0) {
  debug <- isTRUE(debug)
  # Make sure the environment variable is set correctly for debug
  Sys.setenv(LEARNITDOWN_DEBUG = as.integer(debug))

  # Set environment variables according to entries in a crypted configuration
  # file, and return the crypted data, if it succeeds (test database access)
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
    cmd <- switch(.Platform$OS.type, "windows" = "ipconfig", "ifconfig")
    res <- any(grep("(\\d+(\\.|$)){4}", system(cmd, intern = TRUE)))
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
      message("Inaccessible or incorrect configuration or database not responding: ", res)
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
    # If data are already crypted, the string starts with "fingerprint="
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
