#' MD-PMS Exposure Data Frame
#'
#' Converts a data frame into a MD-PMS Exposure data frame.
#'
#' @param data_frame The input data frame requiring components specified in the
#' remaining arguments.
#'
#' @param key Character name of (uniquely identifying) primary key variable in
#' \code{data_frame}. Class must be character or numeric.
#'
#' Example: \code{"key_ID"}
#'
#' Default: \code{NULL} will create a key variable.
#'
#' @param time Character name of date variable in \code{data_frame}. Class must
#' be Date, POSIXt, or character.
#'
#' Example: \code{"event_date"}
#'
#' @param device_hierarchy Vector of character variable names representing the
#' device hierarchy in \code{data_frame}. Vector ordering is lowest level first,
#' most general level last.
#'
#' Example: \code{c("Version", "Device", "ProductLine")}
#'
#' @param event_hierarchy Vector of character variable names representing the
#' event hierarchy in \code{data_frame}. Vector ordering is most specific event
#' category first, most broad event category last.
#'
#' Example: \code{c("Family", "Device", "ProductCode")}
#'
#' Default: \code{NULL} will not include any event hierarchy.
#'
#' @param match_levels Vector of character variable names in \code{data_frame}
#' representing additional grouping factors for exposure. Specified variables
#' will be implicitly matched to equivalently named variables contained in the
#' \code{mdpms.deviceevents} object class.
#'
#' Example: \code{c("Country", "Region")}
#'
#' Default: \code{NULL} will not include any additional grouping factors.
#'
#' @param count Character name of exposure count variable in \code{data_frame}.
#' Class must be numeric.
#'
#' Example: \code{"Units Sold"}
#'
#' Default: \code{NULL} will assume each row represents one exposure.
#'
#' @return A standardized MD-PMS data frame of class \code{mdpms.exposure}.
#' Rows are deduplicated. Attributes are as follows:
#' \describe{
#'   \item{key}{Original variable name for \code{key}}
#'   \item{time}{Original variable name for \code{time}}
#'   \item{device_hierarchy}{Vector of original variable names for
#'   \code{device_hierarchy} with converted variable names correspondingly
#'   named.}
#'   \item{event_hierarchy}{Vector of original variable names for
#'   \code{event_hierarchy} with converted variable names correspondingly
#'   named.}
#'   \item{count}{Original variable name for \code{count}}
#' }
#'
#' @examples
#' ## Need to write!
#'
#' @export
exposure <- function(
  data_frame,
  key=NULL,
  time,
  device_hierarchy,
  event_hierarchy=NULL,
  match_levels=NULL,
  count=NULL
){
  # Check parameters
  input_param_checker(data_frame, check_class="data.frame")
  input_param_checker(key, check_class=c("character", "numeric"),
                      check_names=data_frame)
  input_param_checker(time, check_class=c("character", "POSIXt", "Date"),
                      check_names=data_frame)
  input_param_checker(device_hierarchy, check_class="character",
                      check_names=data_frame)
  input_param_checker(event_hierarchy, check_class="character",
                      check_names=data_frame)
  input_param_checker(match_levels, check_class="character",
                      check_names=data_frame)
  input_param_checker(count, check_class="numeric",
                      check_names=data_frame)
  # Key
  if (is.null(key)){
    v_key <- as.character(c(1:nrow(data_frame)))
  } else{
    v_key <- as.character(data_frame[[key]])
  }
  # Time
  v_time <- as.Date(parsedate::parse_date(data_frame[[time]]))
  # Device Hierarchy
  names(device_hierarchy) <- paste0("device_", c(1:length(device_hierarchy)))
  v_dev <- list()
  for (i in c(1:length(device_hierarchy))){
    v_dev[[names(device_hierarchy)[i]]] <- data_frame[[device_hierarchy[i]]]
  }
  # Event Hierarchy
  if (!is.null(event_hierarchy)){
    names(event_hierarchy) <- paste0("event_", c(1:length(event_hierarchy)))
    v_ev <- list()
    for (i in c(1:length(event_hierarchy))){
      v_ev[[names(event_hierarchy)[i]]] <- data_frame[[event_hierarchy[i]]]
    }
  }
  # Count
  if (is.null(count)){
    ct <- c("count"=NA)
    v_ct <- rep(1, nrow(data_frame))
  } else{
    ct <- c("count"=count)
    v_ct <- data_frame[[count]]
  }
  # Assemble data frame
  dataset <- cbind.data.frame(
    data.frame(key=v_key, time=v_time, count=v_ct),
    data.frame(v_dev))
  if (!is.null(event_hierarchy)){
    dataset <- cbind.data.frame(dataset, data.frame(v_ev))
  }
  # Deduplicate data frame
  dataset <- unique(dataset)
  # Save the output class
  out <- structure(dataset,
                   key=key,
                   time=time,
                   device_hierarchy=device_hierarchy,
                   event_hierarchy=event_hierarchy,
                   match_levels=match_levels,
                   count=count)
  class(out) <- append(class(out), "mdpms.exposure")

  return(out)
}
