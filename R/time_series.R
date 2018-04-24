#' Generate Time Series from Defined Analysis or Analyses
#'
#' Generic function for calculating time series. See
#' \code{\link{time_series.mds_da}} or \code{\link{time_series.mds_das}} as
#' applicable.
#'
#' @param analysis An object of class \code{mds_da} or \code{mds_das}.
#' @param ... Further arguments passed onto \code{time_series} methods,
#' primarily \code{time_series.mds_da()}.
#' @return A time series object or list of time series objects.
#' @seealso \code{\link{time_series.mds_da}}, \code{\link{time_series.mds_das}}
#' @export
time_series <- function(analysis, ...) UseMethod("time_series", analysis)

#' Generate Time Series from Defined Analyses
#'
#' Converts a collection of defined analyses (class \code{mds_das}) to a list
#' of time series objects.
#'
#' @param analysis An object of class \code{mds_das}.
#' @param ... Further arguments passed into \code{\link{time_series.mds_da}}.
#' @return A list of time series objects.
#' @seealso \code{\link{time_series.mds_da}}
#' @export
time_series.mds_das <- function(
  analysis,
  ...
){
  # Create list
  out <- list()
  for(i in 1:length(analysis)){
    out[[i]] <- time_series(analysis=analysis[[i]], ...)
  }
  out
}

#' Generate Time Series from Defined Analysis
#'
#' Converts defined analysis (class \code{mds_da}) to a time series object.
#'
#' @param analysis A defined analysis object of class \code{mds_da}.
#' @param deviceevents A device-events object of class \code{mds_de}. Typically,
#' this will be the same \code{mds_de} object used to generate \code{analysis}.
#' @param exposure Optional exposure object of class \code{mds_e}. Typically,
#' this will be the same \code{mds_e} object used to generate \code{analysis},
#' if an exposure was used.
#'
#' Default: \code{NULL} will not consider exposure data.
#'
#' @param use_hierarchy Logical value indicating whether device and event
#' hierarchies should be used in counting contingency tables for
#' disproportionality analysis. See details for more.
#' @param ... Further arguments for future work.
#'
#' @return A standardized MD-PMS time series data frame of class \code{mds_ts}.
#' The data frame contains, by defined date levels, the following:
#' \describe{
#'   \item{nA}{Count of the device & event level of interest. If covariate
#'   analysis is indicated, this will be at the covariate & device level of
#'   interest.}
#'   \item{nB}{Count of the device & non-event, or if covariate analysis,
#'   covariate & non-device. \code{nB} will be missing if this is an
#'   \code{'All'} level analysis.}
#'   \item{nC}{Count of the non-device & event, or if covariate analysis,
#'   non-covariate & device. \code{nC} will be missing if this is an
#'   \code{'All'} level analysis.}
#'   \item{nD}{Count of the non-device & non-event, or if covariate analysis,
#'   non-covariate & non-device. \code{nD} will be missing if this is an
#'   \code{'All'} level analysis.}
#'   \item{ids}{List of all \code{key}s from \code{deviceevents} constituting
#'   \code{nA}.}
#'   \item{exposure}{Count of exposures applicable to \code{nA}. This counts at
#'   the device and covariate levels but not at the event level. If a matching
#'   device and/or covariate level is not found, then \code{exposure} will be
#'   \code{NA}. The exception is an \code{'All'} level analysis, which counts
#'   exposures across all levels.}
#'   \item{ids_exposure}{List of all exposure keys from \code{exposure}
#'   applicable to \code{nA}.}
#' }
#'
#' Attributes are as follows:
#' \describe{
#'   \item{nA}{Variable name(s) and level(s) (key-value) pair(s) for \code{nA}.}
#'   \item{nABCD}{Variable names and levels (key-value) pairs for the entire
#'   2x2 contingency table, if applicable to the analysis.}
#'   \item{nLabels}{Plain language labels for \code{nA} and, if applicable, the
#'   rows and columns of the 2x2 contingency table.}
#'   \item{exposure}{Boolean of whether exposure counts are present.}
#'   \item{dpa}{Boolean of whether 2x2 contingency table counts are present
#'   (presumably for disproportionality analysis or 'DPA').}
#' }
#'
#' @details When \code{use_hierarchy=T}, the B, C, and D cells of the 2x2
#' contingency table count at the next level up in the device and/or event
#' hierarchies. For example, if the A cell is counting \code{device_1="Apple"}
#' and \code{"Apple"} is the child of the parent \code{device_2="Fruits"}, the
#' B cell counts all \code{"Fruits"} not equal to \code{"Apple"}.
#'
#' When \code{use_hierarchy=F}, the B, C, and D cells of the 2x2 contingency
#' table simply count all devices/events/covariate levels not equal to the
#' reference cell A.
#'
#' @examples
#' ## Need to write!
#'
#' @export
time_series.mds_da <- function(
  analysis,
  deviceevents,
  exposure=NULL,
  use_hierarchy=T,
  ...
){
  # Check parameters
  # ----------------
  input_param_checker(deviceevents, check_class="mds_de")
  input_param_checker(exposure, check_class="mds_e")

  # Set working device-event and exposure data frames
  # -------------------------------------------------
  this <- deviceevents
  if (is.null(exposure)){
    thes <- data.frame()
  } else thes <- exposure

  # Filter device-events and exposure to the relevant levels
  # --------------------------------------------------------
  # Device
  if (analysis$device_level == "All"){
    devlvl <- unique(deviceevents[[names(analysis$device_level)]])
  } else devlvl <- analysis$device_level
  this$isdev <- factor(this[[names(analysis$device_level)]] %in% devlvl,
                       levels=c(T, F))
  if (nrow(thes) > 0 & !is.na(analysis$exp_device_level) &
      analysis$exp_device_level != "All"){
    thes <- thes[thes[[names(analysis$exp_device_level)]] %in%
                   analysis$exp_device_level, ]
  }
  # Event
  if (analysis$event_level == "All"){
    evlvl <- unique(this[[names(analysis$event_level)]])
  } else evlvl <- analysis$event_level
  this$isev <- factor(this[[names(analysis$event_level)]] %in% evlvl,
                      levels=c(T, F))
  # Covariate
  if (analysis$covariate_level != "All"){
    this$iscov <- factor(this[[analysis$covariate]] %in% analysis$covariate_level,
                         levels=c(T, F))
  } else this$iscov <- factor(T, levels=c(T, F))
  if (nrow(thes) > 0 & analysis$covariate_level != "All"){
    if (!is.na(analysis$exp_covariate_level)){
      thes <- thes[thes[[names(analysis$exp_covariate_level)]] %in%
                     analysis$exp_covariate_level, ]
    } else thes <- data.frame()
  }

  # Identify the type of analysis
  # -----------------------------
  if (analysis$covariate_level != "All"){
    # Covariate level analysis
    if (analysis$event_level != "All"){
      # Covariate by event level analysis
      atype <- c("iscov", "isev")
    } else if (analysis$device_level != "All"){
      # Covariate by device level analysis
      atype <- c("iscov", "isdev")
    } else{
      # Covariate only analysis
      atype <- "iscov"
    }
  } else{
    # Non-covariate level analysis
    if (analysis$event_level != "All"){
      # Device by event level analysis
      atype <- c("isdev", "isev")
    } else{
      # Device only analysis
      atype <- "isdev"
    }
  }
  dpa <- length(atype) > 1

  # Filter device-events to 1-level up hierarchy, if needed
  # -------------------------------------------------------
  nextdev <- nextev <- NULL
  if (dpa & use_hierarchy){
    if ("isdev" %in% atype){
      # Filter to 1-level up device hierarchy
      nextdev <- next_dev(names(analysis$device_level))
      if (nextdev %in% names(this)){
        nmiss <- sum(is.na(this[[nextdev]]))
        if (nmiss > 0){
          warning(paste("Dropping", nmiss, "records with missing", nextdev))
          this <- this[!is.na(this[[nextdev]]), ]
        }
        nextdev <- stats::setNames(this[this$isdev == T, ][[nextdev]][1],
                                   nextdev)
        this <- this[this[[names(nextdev)]] == nextdev, ]
      }
    }
    if ("isev" %in% atype){
      # Filter to 1-level up event hierarchy
      nextev <- next_ev(names(analysis$event_level))
      if (nextev %in% names(this)){
        nmiss <- sum(is.na(this[[nextev]]))
        if (nmiss > 0){
          warning(paste("Dropping", nmiss, "records with missing", nextev))
          this <- this[!is.na(this[[nextev]]), ]
        }
        nextev <- stats::setNames(this[this$isev == T, ][[nextev]][1], nextev)
        this <- this[this[[names(nextev)]] == nextev, ]
      }
    }
  }

  # Loop through every deviceevent date period and count
  # ----------------------------------------------------
  tr <- analysis$date_range_de
  ts_de <- data.frame()
  i <- tr[1]
  while (i <= tr[2]){
    j <- attributes(tr)$adder(i, 1)
    thist <- this[this$time >= i & this$time < j, ]
    if (length(atype) > 1){
      # 2-Level Analysis - Populate contingency table
      tbl2x2 <- list(t(stats::setNames(data.frame(table(thist[[atype[1]]],
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

  # Loop through every exposure date period and count
  # -------------------------------------------------
  tr <- analysis$date_range_exposure
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
    exposure <- T
  } else{
    ts <- ts_de
    exposure <- F
  }

  # Define output class attributes
  # ------------------------------
  # Level of nA
  nA <- lapply(atype, function(x){
    if (x == "isdev"){
      analysis$device_level
    } else if (x == "isev"){
      analysis$event_level
    } else if (x == "iscov"){
      stats::setNames(analysis$covariate_level, analysis$covariate)
    }
  })
  # Level of the entire contingency table
  if (dpa){
    nABCD <- lapply(atype, function(x){
      if (x == "isdev"){
        if (is.null(nextdev)){
          stats::setNames("All", names(analysis$device_level))
        } else nextdev
      } else if (x == "isev"){
        if (is.null(nextev)){
          stats::setNames("All", names(analysis$event_level))
        } else nextev
      } else if (x == "iscov"){
        stats::setNames("All", analysis$covariate)
      }
    })
  } else nABCD <- NULL
  # nA-nD English labels
  nLabels <- list(nA=sapply(nA, function(x) paste0(names(x), ":", x)))
  if (dpa){
    nLabels$rows <- paste0(names(nABCD[[1]]), ":", nABCD[[1]])
    nLabels$cols <- paste0(names(nABCD[[2]]), ":", nABCD[[2]])
  }

  # Save the output class
  # ---------------------
  out <- structure(ts,
                   nA=nA,
                   nABCD=nABCD,
                   nLabels=nLabels,
                   exposure=exposure,
                   dpa=dpa)
  class(out) <- append("mds_ts", class(out))

  return(out)
}

# # Debugging
# analysis=testDA[[256]]
# deviceevents=testDE
# exposure=testEX
# use_hierarchy=T

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
