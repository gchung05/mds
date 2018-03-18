#' MD-PMS Device Event Data Frame
#'
#' Converts a data frame into a MD-PMS Device Event data frame.
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
#' Example: \code{c("Event Code", "Event Group")}
#'
#' @param covariates Vector of character variable names representing the
#' desired covariates to retain \code{"_all_"} includes all covariates, assumed
#' to be remaining variables in \code{data_frame} not already specified in
#' \code{key}, \code{time}, \code{device_hierarchy}, or \code{event_hierarchy}.
#'
#' Example: \code{c("Reporter", "City", "Country")}
#'
#' Default: \code{NULL} includes no covariates.
#'
#' @return A standardized MD-PMS data frame of class \code{mdpms.deviceevents}.
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
#'   \item{covariates}{Vector of original variable names for
#'   \code{covariates} with converted variable names correspondingly
#'   named.}
#' }
#'
#' @examples
#' ## Need to write!
#'
#' @export
deviceevents <- function(
  data_frame,
  key=NULL,
  time,
  device_hierarchy,
  event_hierarchy,
  covariates=NULL
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
  input_param_checker(covariates, check_class="character",
                      check_names=data_frame, exclusions="_all_")
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
  names(event_hierarchy) <- paste0("event_", c(1:length(event_hierarchy)))
  v_ev <- list()
  for (i in c(1:length(event_hierarchy))){
    v_ev[[names(event_hierarchy)[i]]] <- data_frame[[event_hierarchy[i]]]
  }
  # Covariates
  key_vars <- c(time, device_hierarchy, event_hierarchy)
  if (!is.null(key)) key_vars <- c(key, key_vars)
  if (is.null(covariates)){
    covs <- NULL
  } else if (all(covariates == "_all_")){
    covs <- names(data_frame)[which(!names(data_frame) %in% key_vars)]
    names(covs) <- make.names(covs)
  } else{
    covs <- covariates
    names(covs) <- make.names(covs)
  }
  if (!is.null(covs)){
    v_cov <- list()
    for (i in c(1:length(covs))){
      v_cov[[names(covs)[i]]] <- data_frame[[covs[i]]]
    }
  }
  # Assemble data frame
  dataset <- cbind.data.frame(
    data.frame(key=v_key, time=v_time),
    data.frame(v_dev),
    data.frame(v_ev))
  if (!is.null(covs)){
    dataset <- cbind.data.frame(dataset, data.frame(v_cov))
  }
  # Deduplicate data frame
  dataset <- unique(dataset)
  # Save the output class
  out <- structure(dataset,
                   key=key,
                   time=time,
                   device_hierarchy=device_hierarchy,
                   event_hierarchy=event_hierarchy,
                   covariates=covs)
  class(out) <- append(class(out), "mdpms.deviceevents")

  return(out)
}
