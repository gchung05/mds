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
#' @param key Character name of (uniquely identifying) primary key variable in
#' \code{data_frame}. Class must be character or numeric.
#'
#' Example: \code{"key_ID"}
#'
#' Default: \code{NULL} will create a key variable.
#'
#' @param covariates Vector of character variable names representing the
#' desired covariates to retain \code{"_all_"} includes all covariates, assumed
#' to be remaining variables in \code{data_frame} not already specified in
#' \code{key}, \code{time}, \code{device_hierarchy}, or \code{event_hierarchy}.
#' It is recommended that covariates are categorical.
#'
#' Example: \code{c("Reporter", "City", "Country")}
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
#' @param implant_days Character name of integer variable in \code{data_frame}
#' representing the days in vivo of the device at the time of the event
#' (\code{time}). More generally, this represents days of exposure of the device
#' at the time of the event.
#'
#' Example: \code{"Implant Days"}. For example, a value of \code{45} indicates
#' that the implant was in vivo for 45 days at the time of the event.
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
  implant_days=NULL
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
  input_param_checker(implant_days, check_class="numeric",
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
    for (i in c(1:length(covs))){
      v_cov[[names(covs)[i]]] <- data_frame[[covs[i]]]
    }
  }
  # Descriptors
  key_vars <- c(time, device_hierarchy, event_hierarchy)
  if (!is.null(key)) key_vars <- c(key, key_vars)
  if (!is.null(covs)) key_vars <- c(covs, key_vars)
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
  if (!is.null(implant_days)){
    v_iday <- list(implant_days=data_frame[[implant_days]])
  }

  # Assemble data frame
  # -------------------
  dataset <- cbind.data.frame(
    data.frame(key=v_key, time=v_time, stringsAsFactors=F),
    data.frame(v_dev),
    data.frame(v_ev))
  if (!is.null(implant_days)) dataset <- cbind.data.frame(dataset,
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
  if (sum(is.na(dataset$device_1)) > 0){
    warning(paste("Dropping", sum(is.na(dataset$device_1)),
                  "rows with missing lowest level device_hierarchy."))
    dataset <- dataset[!is.na(dataset$device_1), ]
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
                   implant_days=implant_days)
  class(out) <- append("mds_de", class(out))

  return(out)
}
