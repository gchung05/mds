#' Check Input Parameters
#'
#' Verifies correct class and, optionally, verifies existence in a data frame
#'
#' @param x Single object name, character, or vector of characters
#' @param check_class Vector of correct classes in character format
#' @param check_names Default: \code{NULL}. Optional data frame to check if
#' \code{x} exists in the data frame.
#' @param null_ok Default: \code{T}. Allow \code{x} to be \code{NULL}. If
#' \code{x} is \code{NULL}, bypass checking.
#' @param exclusions Default: \code{NULL}. Optional values that are excluded
#' from checking in \code{check_names} (if it is not \code{NULL}).
#'
#' @return Stop error, if an error is found. Else nothing.
input_param_checker <- function(
  x,
  check_class=NULL,
  check_names=NULL,
  null_ok=T,
  exclusions=NULL
){
  if (!is.null(exclusions)){
    x <- x[-which(x %in% exclusions)]
  }
  if (length(x) > 0){
    if (length(check_class) > 0){
      if (length(check_names) > 0){
        if (class(x) == "character"){
          this_class <- lapply(check_names[x], class)
        } else this_class <- class(x)
      } else this_class <- class(x)
      if (!any(sapply(this_class, function(x) x %in% check_class))){
        stop(paste0(ifelse(class(x) == "character", paste(x, collapse=" "), quote(x)),
                    " must be of class '", paste(check_class, collapse=" "), "'"))
      }
    }
    if (length(check_names) > 0){
      if(!all(x %in% names(check_names))){
        stop(paste0(x, " names not found in ", check_names))
      }
    }
  }
}
