#' Generate Time Series from Defined Analyses
#'
#' Converts defined analyses into a time series data frame. Accepts both
#' a collection of analyses (Class \code{mdpms.define_analyses}) or a single
#' analysis (Class \code{mdpms.define_analysis}).
#'
#' @param inlist Character name of (uniquely identifying) primary key variable in
#' \code{data_frame}. Class must be character or numeric.
#'
#' Example: \code{"key_ID"}
#'
#' Default: \code{NULL} will create a key variable.
#'
#' @return A standardized MD-PMS data frame of class \code{mdpms.deviceevents}.
#' Rows are deduplicated. Attributes are as follows:
#' \describe{
#'   \item{key}{Original variable name for \code{key}}
#' }
#'
#' @examples
#' ## Need to write!
#'
#' @export
time_series <- function(
  inlist,
  deviceevents,
  exposure=NULL
){
  # Check parameters
  # ----------------
  input_param_checker(inlist, check_class=c("mdpms.define_analysis",
                                            "mdpms.define_analyses"))
  input_param_checker(deviceevents, check_class="mdpms.deviceevents")
  input_param_checker(exposure, check_class="mdpms.exposure")

  # For the initial work, do a single analysis
  inlist=testDA[[1]]
  deviceevents=testDE
  exposure=testEX

  # Set working exposure data frame
  if (is.null(exposure)){
    thes <- data.frame()
  } else thes <- exposure

  # Filter device-events and exposure to the relevant levels
  # --------------------------------------------------------
  # Device
  if (inlist$device_level == "All"){
    devlvl <- unique(deviceevents[[names(inlist$device_level)]])

  } else devlvl <- inlist$device_level
  this <- deviceevents[deviceevents[[names(inlist$device_level)]] %in% devlvl, ]
  if (nrow(thes) > 0 & !is.na(inlist$exp_device_level) &
      inlist$exp_device_level != "All"){
    thes <- thes[thes[[names(inlist$exp_device_level)]] %in%
                   inlist$exp_device_level, ]
  }
  # Event
  if (inlist$event_level == "All"){
    evlvl <- unique(this[[names(inlist$event_level)]])
  } else evlvl <- inlist$event_level
  this <- this[this[[names(inlist$event_level)]] %in% evlvl, ]
  # Covariate
  if (inlist$covariate_level != "All"){
    this <- this[this[[inlist$covariate]] %in% inlist$covariate_level, ]
  }
  if (nrow(thes) > 0 & inlist$covariate_level != "All"){
    if (!is.na(inlist$exp_covariate_level)){
      thes <- thes[thes[[names(inlist$exp_covariate_level)]] %in%
                     inlist$exp_covariate_level, ]
    } else thes <- data.frame()
  }

  # Loop through every deviceevent date period
  tr <- inlist$date_range_de
  i <- tr[1]
  while (i <= tr[2]){
    j <- attributes(tr)$adder(i, 1)
    cat("\n", as.character(as.Date(i, origin="1970-01-01")), "-",
        as.character(as.Date(j, origin="1970-01-01")))
    i <- j
  } # Filter is >= i and < j

  # Loop through every exposure date period
  tr <- inlist$date_range_exposure
  if (!all(is.na(tr))){
    i <- tr[1]
    while (i <= tr[2]){
      j <- attributes(tr)$adder(i, 1)
      cat("\n", as.character(as.Date(i, origin="1970-01-01")), "-",
          as.character(as.Date(j, origin="1970-01-01")))
      i <- j
    } # Filter is >= i and < j

    # When done, merge completed sales counts into main dataframe
  } else{
    # Add in empty exposure columns to main dataframe
  }

  #' Counting the focus level is easy, but how to count the nons?
  #'
  #' For a given: dev, ev
  #' Assuming no cov level (Data All case)
  #' ---------------------
  #' For each time period, 4 numbers are needed
  #' 1. the dev ev | 2., 3., 4. the non-dev, non-ev (these shall not req ids)
  #' Non-dev are all devices belonging to the same one-level up dev hierarchy
  #' Non-ev are all events belonging to the same one-level up ev hierarchy
  #' UNLESS: any dev/ev level is "All", then NA's for 2-4.
  #'
  #' For a given: dev, ev, cov
  #' -------------------------
  #' If a specific dev and ev
  #' The cov level and ev|dev are n11, 2-4. are non-cov and non-ev within dev
  #' If specific dev and all ev
  #' The cov level and dev are n11, 2-4. are non-cov and non-dev (same 1-lvl up hierarchy)
  #' UNLESS: dev and ev are both all, then NA's for 2-4.

  # Save the output class
  # ---------------------
  out <- structure(dataset,
                   key=key,
                   time=time,
                   device_hierarchy=device_hierarchy,
                   event_hierarchy=event_hierarchy,
                   covariates=covs,
                   descriptors=dscr)
  class(out) <- append("mdpms.time_series", class(out))

  return(out)
}
