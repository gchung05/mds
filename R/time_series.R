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
#' @param deviceevents blah
#'
#' @param exposure blah
#'
#' @param use_hierarchy blah
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
  exposure=NULL,
  use_hierarchy=T
){
  # Check parameters
  # ----------------
  input_param_checker(inlist, check_class=c("mdpms.define_analysis",
                                            "mdpms.define_analyses"))
  input_param_checker(deviceevents, check_class="mdpms.deviceevents")
  input_param_checker(exposure, check_class="mdpms.exposure")

  # For the initial work, do a single analysis
  inlist=testDA[[40]]
  deviceevents=testDE
  exposure=testEX
  use_hierarchy=T

  # I AM HERE!!!!!
  # OK well testDA[[40]] failed.... check it....


  # Set working device-event and exposure data frames
  this <- deviceevents
  if (is.null(exposure)){
    thes <- data.frame()
  } else thes <- exposure

  # Filter device-events and exposure to the relevant levels
  # --------------------------------------------------------
  # Device
  if (inlist$device_level == "All"){
    devlvl <- unique(deviceevents[[names(inlist$device_level)]])
  } else devlvl <- inlist$device_level
  this$isdev <- factor(this[[names(inlist$device_level)]] %in% devlvl,
                       levels=c(T, F))
  if (nrow(thes) > 0 & !is.na(inlist$exp_device_level) &
      inlist$exp_device_level != "All"){
    thes <- thes[thes[[names(inlist$exp_device_level)]] %in%
                   inlist$exp_device_level, ]
  }
  # Event
  if (inlist$event_level == "All"){
    evlvl <- unique(this[[names(inlist$event_level)]])
  } else evlvl <- inlist$event_level
  this$isev <- factor(this[[names(inlist$event_level)]] %in% evlvl,
                      levels=c(T, F))
  # Covariate
  if (inlist$covariate_level != "All"){
    this$iscov <- factor(this[[inlist$covariate]] %in% inlist$covariate_level,
                         levels=c(T, F))
  } else this$iscov <- factor(T, levels=c(T, F))
  if (nrow(thes) > 0 & inlist$covariate_level != "All"){
    if (!is.na(inlist$exp_covariate_level)){
      thes <- thes[thes[[names(inlist$exp_covariate_level)]] %in%
                     inlist$exp_covariate_level, ]
    } else thes <- data.frame()
  }

  # Identify the type of analysis
  # -----------------------------
  if (inlist$covariate_level != "All"){
    # Covariate level analysis
    if (inlist$event_level != "All"){
      # Covariate by event level analysis
      atype <- c("iscov", "isev")
    } else if (inlist$device_level != "All"){
      # Covariate by device level analysis
      atype <- c("iscov", "isdev")
    } else{
      # Covariate only analysis
      atype <- "iscov"
    }
  } else{
    # Non-covariate level analysis
    if (inlist$event_level != "All"){
      # Device by event level analysis
      atype <- c("isdev", "isev")
    } else{
      # Device only analysis
      atype <- "isdev"
    }
  }

  # Filter device-events to 1-level up hierarchy, if needed
  # -------------------------------------------------------
  if (length(atype) > 1 & use_hierarchy){
    if ("isdev" %in% atype){
      # Filter to 1-level up device hierarchy
      nextdev <- next_dev(names(inlist$device_level))
      if (nextdev %in% names(this)){
        nmiss <- sum(is.na(this[[nextdev]]))
        if (nmiss > 0){
          warning(paste("Dropping", nmiss, "records with missing", nextdev))
          this <- this[!is.na(this[[nextdev]]), ]
        }
        this <- this[this[[nextdev]] == this[this$isdev == T, ][[nextdev]][1], ]
      }
    }
    if ("isev" %in% atype){
      # Filter to 1-level up event hierarchy
      nextev <- next_ev(names(inlist$event_level))
      if (nextev %in% names(this)){
        nmiss <- sum(is.na(this[[nextev]]))
        if (nmiss > 0){
          warning(paste("Dropping", nmiss, "records with missing", nextev))
          this <- this[!is.na(this[[nextev]]), ]
        }
        this <- this[this[[nextev]] == this[this$isev == T, ][[nextev]][1], ]
      }
    }
  }

  # Loop through every deviceevent date period
  # ------------------------------------------
  tr <- inlist$date_range_de
  ts_de <- data.frame()
  i <- tr[1]
  while (i <= tr[2]){
    j <- attributes(tr)$adder(i, 1)
    thist <- this[this$time >= i & this$time < j, ]
    if (length(atype) > 1){
      # 2-Level Analysis - Populate contingency table
      tbl2x2 <- list(t(setNames(data.frame(table(thist[[atype[1]]],
                                                 thist[[atype[2]]]))[, 3],
                                c("nA", "nC", "nB", "nD"))))
      trow <- data.frame(time=i, tbl2x2)
      trow$ids <- list(unique(
        as.character(thist[thist[[atype[1]]] == T &
                             thist[[atype[2]]] == T, ]$key)))
    } else{
      # 1-Level Analysis - No contingency table
      trow <- data.frame(time=i, nA=table(thist[[atype[1]]])['TRUE'])
      trow$ids <- list(unique(as.character(thist[thist[[atype[1]]] == T, ]$key)))
    }
    ts_de <- rbind(ts_de, trow)
    i <- j
  }
  rownames(ts_de) <- c()

  # Loop through every exposure date period
  # ---------------------------------------
  tr <- inlist$date_range_exposure
  if (!all(is.na(tr)) & nrow(thes) > 0){
    ts_e <- data.frame()
    i <- tr[1]
    while (i <= tr[2]){
      j <- attributes(tr)$adder(i, 1)
      thest <- thes[thes$time >= i & thes$time < j, ]
      trow <- data.frame(time=i, exposure=sum(thest$count))
      trow$ids_exposure <- list(as.character(thest$key))
      ts_e <- rbind(ts_e, trow)
      i <- j
    }
    # Merge device-events and exposure time series
    ts <- merge(ts_de, ts_e, all=T)
  } else{
    ts <- ts_de
  }

  #' Attributes to save:
  #' nA definition: what is/are the variables used (variable:value pairs)
  #' nB-nD: Variable hierarchy used (variable:value pairs). If no hierarchy used, NULL
  #' nA-nD: English labels
  #' maybe: tr?
  #' Analysis ID
  #' Exposures or not
  #' DPA or not

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
