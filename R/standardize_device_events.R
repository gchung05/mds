#' MD-PMS Device Event Data Frame
#'
#' Converts a data frame into a MD-PMS Device Event data frame.
#'
#' @param data_frame The input data frame requiring components specified in the
#' remaining arguments.
#'
#' @param time Character name of date variable in \code{data_frame}
#' corresponding to the event. Class must be Date, POSIXt, or character.
#'
#' Example: \code{"event_date"}
#'
#' @param device_hierarchy Vector of character variable names representing the
#' device hierarchy in \code{data_frame}. Vector ordering is lowest level first,
#' most general level last. If more than 2 variables, see important note in 
#' Details.
#'
#' Example: \code{c("Version", "Device", "ProductLine")}
#'
#' @param event_hierarchy Vector of character variable names representing the
#' event hierarchy in \code{data_frame}. Vector ordering is most specific event
#' category first, most broad event category last. If more than 2 variables,
#' see important note in Details.
#'
#' Example: \code{c("Event Code", "Event Group")}
#'
#' @param key Character name of (uniquely identifying) primary key variable in
#' \code{data_frame}. Class must be character or numeric.
#'
#' Example: \code{"key_ID"}
#'
#' Default: \code{NULL} will create a key variable.
#'
#' @param covariates Vector of character variable names representing the
#' desired covariates to retain, all of which must be of class \code{numeric}
#' or \code{factor}.  \code{"_all_"} includes all covariates, assumed
#' to be remaining variables in \code{data_frame} not already specified in
#' \code{key}, \code{time}, \code{device_hierarchy}, or \code{event_hierarchy}.
#' Covariates must be numeric, categorical, or binary in nature.
#'
#' Example: \code{c("Reporter", "Operation Time", "Country")}
#'
#' Default: \code{NULL} includes no covariates.
#'
#' @param descriptors Vector of character variable names representing additional
#' descriptive variables that will not be used in any analyses but may be
#' recalled or displayed later during individual device-event review.
#' \code{"_all_"} includes all remaining variables in \code{data_frame} not
#' already specified in \code{key}, \code{time}, \code{device_hierarchy},
#' \code{event_hierarchy}, or \code{covariates}. Typical descriptors are
#' free text or high-dimensional categoricals.
#'
#' Example: \code{c("Description", "Unique Device Identifier")}
#'
#' Default: \code{NULL} includes no descriptors.
#'
#' @param time_invivo Character name of numeric variable in \code{data_frame}
#' representing the time in vivo of the device at the time of the event
#' \code{time}. See details for more.
#'
#' IMPORTANT: If a call to \code{define_analyses()} is planned,
#' \code{time_invivo} must be in the time units specified collectively by its
#' parameters \code{date_level} and \code{date_level_n}.
#'
#' Example: \code{"Implanted Months"}. A value of \code{45} in the
#' variable \code{data_frame$'Implanted Months'} would indicate 45 units of time
#' elapsed since the device was first in vivo. If \code{date_level="months"} and
#' \code{date_level_n=1}, this will be interpreted by \code{define_analyses()}
#' as 45 months.
#'
#' Default: \code{NULL} indicates this variable will not be used.
#'
#' @return A standardized MD-PMS data frame of class \code{mds_de}.
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
#'   \item{descriptors}{Vector of original variable names for
#'   \code{descriptors} with converted variable names correspondingly
#'   named.}
#' }
#'
#' @details
#' When more than 2 variables are specified in either \code{device_hierarchy} 
#' or \code{event_hierarchy}, it is important to note that a subsequent call to
#' \code{define_analyses()} currently only utilizes a maximum of 2 variables: 
#' the lowest level and the 1-level-up parent. The user may enforce full
#' hierarchy in >2 variable cases by ensuring that the parent values are
#' uniquely named.
#' 
#' \code{time_invivo} can be thought of more generally as the time of
#' exposure of the device to the subject at the time of the event. The common
#' usage is duration of the implant in the patient at time of event, for an
#' implantable medical device.
#'
#' @examples
#' # A barebones dataset
#' de <- deviceevent(maude, "date_received", "device_name", "event_type")
#' # With more variables and variable types
#' de <- deviceevent(
#'   data_frame=maude,
#'   time="date_received",
#'   device_hierarchy=c("device_name", "device_class"),
#'   event_hierarchy=c("event_type", "medical_specialty_description"),
#'   key="report_number",
#'   covariates=c("region"),
#'   descriptors="_all_")
#'
#' @export
deviceevent <- function(
  data_frame,
  time,
  device_hierarchy,
  event_hierarchy,
  key=NULL,
  covariates=NULL,
  descriptors=NULL,
  time_invivo=NULL
){
  # Check parameters
  # ----------------
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
  input_param_checker(descriptors, check_class="character",
                      check_names=data_frame, exclusions="_all_")
  input_param_checker(time_invivo, check_class="numeric",
                      check_names=data_frame, max_length=1)
  
  # Address each variable
  # ---------------------
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
    covs <- stats::setNames(covariates, make.names(covariates))
  }
  if (!is.null(covs)){
    v_cov <- list()
    # Must drop any covariates that are not numeric or factors
    a <- unlist(lapply(data_frame[, as.character(covs), drop=F], is.numeric))
    b <- unlist(lapply(data_frame[, as.character(covs), drop=F], is.factor))
    if (any(!(a | b))){
      bad_covs <- names(a)[!(a | b)]
      warning(paste0("Non-numeric and non-factor covariates moved to descriptors: ",
                     paste(bad_covs, collapse=", ")))
      covs <- covs[!as.character(covs) %in% bad_covs]
    }
    if (length(covs) > 0){
      for (i in c(1:length(covs))){
        v_cov[[names(covs)[i]]] <- data_frame[[covs[i]]]
      }
    } else covs <- NULL
  }
  # Descriptors
  key_vars <- c(time, device_hierarchy, event_hierarchy)
  if (!is.null(key)) key_vars <- c(key, key_vars)
  if (!is.null(covs)) key_vars <- c(covs, key_vars)
  if (!is.null(time_invivo)) key_vars <- c(time_invivo, key_vars)
  if (is.null(descriptors)){
    dscr <- NULL
  } else if (all(descriptors == "_all_")){
    dscr <- names(data_frame)[which(!names(data_frame) %in% key_vars)]
    names(dscr) <- make.names(dscr)
  } else{
    dscr <- stats::setNames(descriptors, make.names(descriptors))
  }
  if (!is.null(dscr)){
    v_dsc <- list()
    for (i in c(1:length(dscr))){
      v_dsc[[names(dscr)[i]]] <- data_frame[[dscr[i]]]
    }
  }
  # Implant Days
  if (!is.null(time_invivo)){
    v_iday <- list(time_invivo=data_frame[[time_invivo]])
  }

  # Assemble data frame
  # -------------------
  dataset <- cbind.data.frame(
    data.frame(key=v_key, time=v_time, stringsAsFactors=F),
    data.frame(v_dev),
    data.frame(v_ev))
  if (!is.null(time_invivo)) dataset <- cbind.data.frame(dataset,
                                                          data.frame(v_iday))
  if (!is.null(covs)) dataset <- cbind.data.frame(dataset, data.frame(v_cov))
  if (!is.null(dscr)){
    for(i in 1:length(v_dsc)){
      if ("list" %in% class(v_dsc[[i]])){
        thiscol <- data.frame(I(v_dsc[[i]]))
      } else{
        thiscol <- data.frame(v_dsc[[i]], stringsAsFactors=F)
      }
      names(thiscol) <- names(v_dsc[i])
      if (!exists("allcol")){
        allcol <- thiscol
      } else allcol <- cbind(allcol, thiscol)
    }
    dataset <- cbind.data.frame(dataset, allcol)
  }

  # Cleanup
  # -------
  # Deduplicate data frame
  dataset <- unique(dataset)
  # Drop rows with missing required fields
  # Missing time
  if (sum(is.na(dataset$time)) > 0){
    warning(paste("Dropping", sum(is.na(dataset$time)),
                  "rows with missing time."))
    dataset <- dataset[!is.na(dataset$time), ]
  }
  # Missing device levels
  for (i in names(v_dev)){
    if (sum(is.na(dataset[[i]])) > 0){
      warning(paste("Dropping", sum(is.na(dataset[[i]])),
                    "rows with missing", i, "device_hierarchy."))
      dataset <- dataset[!is.na(dataset[[i]]), ]
    }
  }
  # Missing event levels
  for (i in names(v_ev)){
    if (sum(is.na(dataset[[i]])) > 0){
      warning(paste("Dropping", sum(is.na(dataset[[i]])),
                    "rows with missing", i, "event_hierarchy."))
      dataset <- dataset[!is.na(dataset[[i]]), ]
    }
  }
  # Missing covariate levels
  if (!is.null(covs)){
    for (i in names(v_cov)){
      if (sum(is.na(dataset[[i]])) > 0){
        warning(paste("Dropping", sum(is.na(dataset[[i]])),
                      "rows with missing", i, "covariate"))
        dataset <- dataset[!is.na(dataset[[i]]), ]
      }
    }
  }

  # Save the output class
  # ---------------------
  out <- structure(dataset,
                   time=time,
                   device_hierarchy=device_hierarchy,
                   event_hierarchy=event_hierarchy,
                   key=key,
                   covariates=covs,
                   descriptors=dscr,
                   time_invivo=time_invivo)
  class(out) <- append("mds_de", class(out))

  return(out)
}
