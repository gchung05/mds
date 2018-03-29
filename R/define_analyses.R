#' Set MD-PMS Analyses Definitions
#'
#' Define and enumerate analyses based on MS-PMS device-event data frames and
#' (optionally) MD-PMS exposure data frames.
#'
#' @param key Character name of (uniquely identifying) primary key variable in
#' \code{data_frame}. Class must be character or numeric.
#'
#' Example: \code{"key_ID"}
#'
#' Default: \code{NULL} will create a key variable.
#'
#' @return A list of defined analyses of class \code{mdpms.analyses}.
#' Attributes are as follows:
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
define_analyses <- function(
  deviceevents,
  exposure=NULL,
  algorithms=NULL,
  date_level="month",
  device_levels="device_1",
  event_levels=NULL,
  times_to_calc=NULL,
  prior=NULL
){
  # # What do I want next?
  # foo(deviceevents=testDE,
  #     exposure=NULL, # or specify mdpms.exposure object
  #     algorithms=NULL, # Default NULL just counts, otherwise several keywords
  #     date_level="month", # Default is month, otherwise day, quarter, semiannual, annual
  #     device_level=1, # Otherwise specify number > 1
  #     event_level=1, # Otherwise specify number > 1,
  #     times_to_calc=NULL, # Null calculates all time, otherwise a number of units at the date level
  #     prior=NULL # A prior foo object. If specified, will only calculate new stuff (by time). If specified, times_to_calc is ignored.
  # )

  # Current possible algorithms where count is the default
  # algos <- c("count", "Shewhart-WE1", "EWMA-WE1", "PRR")

  # Current possible date levels
  date_levs <- data.frame(month=NA, day=NA, quarter=NA, semiannual=NA,
                          annual=NA)

  # Check parameters
  input_param_checker(deviceevents, check_class="mdpms.deviceevents")
  input_param_checker(exposure, check_class="mdpms.exposure")
  # input_param_checker(algorithms, check_class="character",) # WILL NEED TO WORRY ABOUT THIS SOON
  input_param_checker(date_level, check_class="character",
                      check_names=date_levs)
  input_param_checker(device_levels, check_class="character",
                      check_names=attributes(deviceevents)$device_hierarchy)
  input_param_checker(event_levels, check_class="character",
                      check_names=attributes(deviceevents)$event_hierarchy)
  input_param_checker(times_to_calc, check_class="numeric")
  input_param_checker(prior, check_class="mdpms.analyses")

  # I AM HERE!!!!!!!!!
  # enumerate devices, events, exposures
  # enumerate times
  # compare to prior
  # if exposure exists, account for overlap
  # define output structure

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
