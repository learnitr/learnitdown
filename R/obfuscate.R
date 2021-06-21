#' Obfuscate answers in learnr documents
#'
#' @param object An R object to obfuscate, or Obfuscation string to decrypt.
#' @param password The password to use (otherwise, the password defined by the
#' application is used)
#' @param x A logical value or an integer.
#' @param y Obfuscation parameter.
#' @param r Reset obfuscation (not intended for end-user)?
#' @param correct Is the answer is correct or not (either a logical value, or
#' an integer that obfuscates the results).
#' @param text Text of the option.
#' @param message Message displayed if the item is selected
#'
#' @return
#' The encrypted or decrypted object.
#' @export
#'
#' @examples
#' obfuscate("test", "password")
obfuscate <- function(object, password) {
  if (!missing(password)) {
    lock(object, password)
  } else {
    args <- getOption("learnitdown_obfuscation", list(key = "",
      message = "Password:"))
    lock(object, key = args$key, message = args$message)
  }
}

#' @rdname obfuscate
#' @export
._ <- function(object) {
  args <- getOption("learnitdown_obfuscation", list(key = "",
    message = "Password:", admin = "V6v4wBAeFTX5tkAIvuRR5A==", user = "user"))
  if (interactive()) {
    unlock(object, key = args$key, message = args$message)
  } else {
    unlock(object, unlock(args$admin, args$user))
  }
}

.get_obfuscator <- function(reset = FALSE) {
  args <- getOption("learnitdown_obfuscator", list(key = "",
    message = "Password:", admin = "V6v4wBAeFTX5tkAIvuRR5A==", user = "user"))
  if (interactive()) {
    .get_password(key = args$key, message = args$message, reset = isTRUE(reset),
      ref1 = args$admin, ref2 = args$user)
  } else {
    unlock(args$admin, args$user)
  }
}

.gh=round;.kq=abs;.kw=function(x) nchar(x);.zc=missing;.ls=runif;.ty=is.logical
.bg=function(x) charToRaw(x);.ho=as.integer;.ud=.get_obfuscator;.fy=isTRUE
.rg=function(x, times) strrep(x, times)
.dx=function(test, yes, no) ifelse(test, yes, no)

#' @rdname obfuscate
#' @export
obfuscate_logical <- function(x, y, r = FALSE) {
  w=1L;W=100L;y=.dx(.zc(y),.ud(r),y);.dx(.ty(x),{z=.gh(.ls(w,w,W));.dx((.ho(.bg(
  .rg(y,(W%/%.kw(y))+w))[z])%%2L==.fy(x)),z,-z)},{v=TRUE;z=.ho(.bg(.rg(y,(W%/%
  .kw(y))+w))[.kq(x)])%%2L==v;.dx(x>=w,z,!z)})
}

#' @rdname obfuscate
#' @export
ans <- function(correct, text, message = NULL) {
  #if (!is.logical(correct))
  #  correct <- obfuscate_logical(correct)
  if (interactive()) {
    if (is.null(message)) {
      message("answer(", text, ", correct = ", correct, ")")
    } else {
      message("answer(", text, ", correct = ", correct,
        ", message = ", message, ")")
    }

  }
  # This is learnr::answer()
  answer(text = text, correct = correct, message = message)
}
