#' @importFrom dplyr "%>%"
#' @importFrom lubridate "%m+%"

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
#' @param max_length Default: \code{NULL}. Optional maximum length of \code{x}.
#' If \code{NULL}, no max length check will occur.
#'
#' @return Stop error, if an error is found. Else nothing.
input_param_checker <- function(
  x,
  check_class=NULL,
  check_names=NULL,
  null_ok=T,
  exclusions=NULL,
  max_length=NULL
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
      if (!all(x %in% names(check_names))){
        stop(paste0(x, " names not found in ", check_names))
      }
    }
    if (!is.null(max_length)){
      if (length(x) > max_length){
        stop(paste0(x, " cannot exceed maximum length of ", max_length))
      }
    }
  }
}

#' Convert to Acceptable Date
#'
#' Converts a \code{Date} vector into into its equivalent daily, monthly, yearly,
#' etc... \code{Date} vector.
#'
#' @param x Input vector of class \code{Date}.
#' @param convert_to Default: \code{"month"}. String value indicating the
#' conversion. Current acceptable conversions are \code{"day"}, \code{"month"},
#' \code{"quarter"}, \code{"semiannual"}, \code{"annual"}.
#'
#' @return Converted \code{Date} vector of class \code{mdpms.Date} with
#' attributes \code{adder} (function that adds units),
#' \code{current_conversion}, and \code{possible_conversions}.
convert_date <- function(
  x,
  convert_to="month"
){
  # Acceptable conversions
  converts <- c("day", "month", "quarter", "semiannual", "annual")
  if (convert_to == converts[1]){
    # Day
    this <- x
    adder <- function(t, n){
      t + lubridate::ddays(n)
    }
    duration <- lubridate::ddays(1)
  } else if (convert_to == converts[2]){
    # Month
    this <- lubridate::floor_date(x, "month")
    adder <- function(t, n){
      t %m+% months(n)
    }
  } else if (convert_to == converts[3]){
    # Quarter
    this <- lubridate::floor_date(x, "quarter")
    adder <- function(t, n){
      t %m+% months(3 * n)
    }
  } else if (convert_to == converts[2]){
    # Semiannual
    this <- lubridate::floor_date(x, "halfyear")
    adder <- function(t, n){
      t %m+% months(6 * n)
    }
  } else if (convert_to == converts[2]){
    # Annual
    this <- lubridate::floor_date(x, "year")
    adder <- function(t, n){
      t + lubridate::dyears(n)
    }
  }
  # Save the output class
  out <- structure(this,
                   current_conversion=convert_to,
                   possible_conversions=converts,
                   adder=adder)
  class(out) <- append("mdpms.Date", class(out))
  return(out)
}
