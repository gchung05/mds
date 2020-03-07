#' Generate Time Series from Defined Analysis or Analyses
#'
#' Creates time series data frame(s) from defined analysis/analyses
#' (\code{define_analyses()}), device-event data frame
#' (\code{deviceevent()}), and optionally, exposure data frame
#' (\code{exposure()}). If analysis includes covariates or time in-vivo, creates
#' the relevant supporting data frame.
#'
#' @param analysis A defined analysis object of class \code{mds_da}, list of
#' class \code{mds_das}, or a list of objects each of class \code{mds_da},
#' usually created by \code{define_analyses()}.
#' @param deviceevents A device-event data frame of class \code{mds_de}, usually
#' created by \code{deviceevent()}. This should be the same data frame used to
#' generate \code{analysis}.
#' @param exposure Optional exposure data frame of class \code{mds_e}, usually
#' created by \code{exposure()}. This should be the same data frame used to
#' generate \code{analysis}, if exposure data was used.
#'
#' Default: \code{NULL} will not consider exposure data.
#'
#' @param use_hierarchy Deprecated - do not use. Logical value indicating
#' whether device and event hierarchies should be used in counting contingency
#' tables for disproportionality analysis.
#'
#' @param ... Further arguments for future work.
#'
#' @return A standardized MD-PMS time series data frame of class \code{mds_ts}.
#'
#' The data frame contains, by defined date levels, the following columns:
#' \describe{
#'   \item{nA}{Count of the device & event level of interest. If covariate
#'   analysis is indicated, this will be at the covariate & device level of
#'   interest.}
#'   \item{nB}{Optional. Count of the device & non-event, or if covariate analysis,
#'   covariate & non-device. \code{nB} will be missing if this is an
#'   \code{'All'} level analysis.}
#'   \item{nC}{Optional. Count of the non-device & event, or if covariate analysis,
#'   non-covariate & device. \code{nC} will be missing if this is an
#'   \code{'All'} level analysis.}
#'   \item{nD}{Optional. Count of the non-device & non-event, or if covariate analysis,
#'   non-covariate & non-device. \code{nD} will be missing if this is an
#'   \code{'All'} level analysis.}
#'   \item{ids}{List of all \code{key}s from \code{deviceevents} constituting
#'   \code{nA}.}
#'   \item{exposure}{Optional. Count of exposures applicable to \code{nA}. This counts at
#'   the device and covariate levels but not at the event level. If a matching
#'   device and/or covariate level is not found, then \code{exposure} will be
#'   \code{NA}. The exception is an \code{'All'} level analysis, which counts
#'   exposures across all levels.}
#'   \item{ids_exposure}{Optional. List of all exposure keys from \code{exposure}
#'   applicable to \code{nA}.}
#' }
#'
#' The \code{mds_ts} class attributes are as follows:
#' \describe{
#'   \item{title}{Short description of the analysis.}
#'   \item{analysis}{The analysis definition of class \code{mds_da}.}
#'   \item{exposure}{Boolean of whether exposure counts are present.}
#'   \item{dpa}{Boolean of whether 2x2 contingency table counts are present
#'   (presumably for disproportionality analysis or 'DPA').}
#'   \item{dpa_detail}{Optional. If \code{dpa} is \code{TRUE}, \code{list}
#'   object containing labels for the DPA contingency table.}
#'   \item{covar_data}{Optional. If analysis definition includes covariate level
#'   or time in-vivo, \code{data.frame} object containing the relevant data.}
#' }
#'
#' @examples
#' de <- deviceevent(maude, "date_received", "device_name", "event_type")
#' ex <- exposure(sales, "sales_month", "device_name", count="sales_volume")
#' da <- define_analyses(de, "device_name", exposure=ex)
#' # Time series on one analysis
#' time_series(da, de, ex)
#' # Time series on multiple analyses
#' time_series(da[1:3], de, ex)
#'
#' @export
time_series <- function(analysis, ...){
  UseMethod("time_series", analysis)
}

#' @describeIn time_series Generate time series from a list
#' @export
time_series.list <- function(
  analysis,
  ...
){
  # Dispatch to correct method based on list search
  if ("mds_da" %in% class(analysis)){
    time_series.mds_da(analysis=analysis, ...)
  } else if ("mds_das" %in% class(analysis)){
    time_series.mds_das(analysis=analysis, ...)
  } else if (all(sapply(analysis, function(x) "mds_da" %in% class(x)))){
    time_series.mds_das(analysis=analysis, ...)
  } else{
    stop("Analysis object not valid. See ?time_series")
  }
}


#' @describeIn time_series Generate time series from a list of defined analyses
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

#' @describeIn time_series Generate time series using defined analysis
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
  if (isFALSE(use_hierarchy)) {
    warning("argument use_hierarchy is deprecated, please discontinue use.",
            call. = FALSE)
  }

  # Set working device-event and exposure data frames
  # -------------------------------------------------
  this <- deviceevents
  if (is.null(exposure)){
    thes <- data.frame()
  } else thes <- exposure

  # Filter/flag device-events and exposure to the relevant levels
  # -------------------------------------------------------------
  # Device - Primary
  if (analysis$device_level == "All"){
    devlvl <- unique(deviceevents[[names(analysis$device_level)]])
  } else devlvl <- analysis$device_level
  this$isdev <- as.logical(this[[names(analysis$device_level)]] %in% devlvl)
  if (nrow(thes) > 0 & !is.na(analysis$exp_device_level) &
      analysis$exp_device_level != "All"){
    thes <- thes[thes[[names(analysis$exp_device_level)]] %in%
                   analysis$exp_device_level, ]
  }
  # Event - Primary
  if (analysis$event_level == "All"){
    evlvl <- unique(this[[names(analysis$event_level)]])
  } else evlvl <- analysis$event_level
  this$isev <- as.logical(this[[names(analysis$event_level)]] %in% evlvl)
  # Covariate
  if (is.na(analysis$covariate_level)){ # NA nominal level
    this$iscov <- as.logical(is.na(this[[analysis$covariate]]))
  } else if (analysis$covariate_level != "All"){ # Nominal level
    this$iscov <- as.logical(this[[analysis$covariate]] %in%
                           analysis$covariate_level)
  } else this$iscov <- as.logical(T) # Marginal, Data All level & numeric
  if (nrow(thes) > 0){
    if (!is.null(analysis$exp_covariate_level)){
      if (is.na(analysis$exp_covariate_level)){ # NA nominal level
        thes <- thes[is.na(thes[[names(analysis$exp_covariate_level)]]), ]
      } else if (analysis$exp_covariate_level != "All"){ # Nominal level
        thes <- thes[thes[[names(analysis$exp_covariate_level)]] %in%
                       analysis$exp_covariate_level, ]
      } # Non-covariate exposures or covariate Marginal, Data All, numeric: do not filter
    }
  }

  # # Identify the type of analysis
  # # -----------------------------
  #
  # ################
  # # need to restructure this altogether. cannot do a paired analysis at all
  # # especially not a X by covariate analysis now that numeric covariates are allowed
  # # DPA should be inferred from the levels available in device and event only
  # # Thus DPA is not possible at either All level of device or event
  #
  # # Note: In the future for covariate by device by event level analysis (3D),
  # # atype may be modified to return a vector of c("iscov", "isdev", "isev")
  # if (analysis$covariate_level != "All"){
  #   # Covariate level analysis
  #   if (analysis$event_level != "All"){
  #     # Covariate by event level analysis
  #     atype <- stats::setNames(c("iscov", "isev"), c("Covariate", "Event"))
  #   } else if (analysis$device_level != "All"){
  #     # Covariate by device level analysis
  #     atype <-  stats::setNames(c("iscov", "isdev"), c("Covariate", "Device"))
  #   } else{
  #     # Covariate only analysis
  #     atype <-  stats::setNames("iscov", c("Covariate"))
  #   }
  # } else{
  #   # Non-covariate level analysis
  #   if (analysis$event_level != "All"){
  #     # Device by event level analysis
  #     atype <-  stats::setNames(c("isdev", "isev"), c("Device", "Event"))
  #   } else{
  #     # Device only analysis
  #     atype <- stats::setNames("isdev", c("Device"))
  #   }
  # }
  # dpa <- length(atype) > 1
  # ################
  # # Dpa should now be isdev and isev? How?

  # Filter to 1-level up hierarchy, if needed
  # -----------------------------------------
  # Filter device-events to 1-level up device hierarchy
  nextdev <- next_dev(names(analysis$device_level))
  if (!is.na(analysis$device_1up)){
    if (nextdev %in% names(this) & nextdev == names(analysis$device_1up)){
      nmiss <- sum(is.na(this[[nextdev]]))
      if (nmiss > 0){
        warning(paste("Dropping", nmiss, "records with missing", nextdev))
        this <- this[!is.na(this[[nextdev]]), ]
      }
      this <- this[this[[nextdev]] %in% analysis$device_1up, ]
      # Filter exposures to 1-level up device hierarchy
      if (!is.na(analysis$exp_device_1up)){
        if (nrow(thes) > 0 & analysis$exp_device_1up != "All" &
            nextdev == names(analysis$exp_device_1up)){
          thes <- thes[thes[[nextdev]] %in% analysis$exp_device_1up, ]
        }
      }
    }
  }
  # Filter device-events to 1-level up event hierarchy
  nextev <- next_ev(names(analysis$event_level))
  if (!is.na(analysis$event_1up)){
    if (nextev %in% names(this) & nextev == names(analysis$event_1up)){
      nmiss <- sum(is.na(this[[nextev]]))
      if (nmiss > 0){
        warning(paste("Dropping", nmiss, "records with missing", nextev))
        this <- this[!is.na(this[[nextev]]), ]
      }
      this <- this[this[[nextev]] %in% analysis$event_1up, ]
    }
    # Exposures are not currently filtered for events
  }

  # Assess possibility for disproportionality
  # Based on device and event level only
  # -----------------------------------------
  dpa <- length(unique(this$isdev)) == 2 & length(unique(this$isev)) == 2

  # nextdev <- nextev <- NULL
  # if (dpa & use_hierarchy){
  #   if ("isdev" %in% atype){
  #     # Filter device-events to 1-level up device hierarchy
  #     nextdev <- next_dev(names(analysis$device_level))
  #     if (nextdev %in% names(this) & !is.na(analysis$device_1up) &
  #       nextdev == names(analysis$device_1up)){
  #       nmiss <- sum(is.na(this[[nextdev]]))
  #       if (nmiss > 0){
  #         warning(paste("Dropping", nmiss, "records with missing", nextdev))
  #         this <- this[!is.na(this[[nextdev]]), ]
  #       }
  #       this <- this[this[[nextdev]] %in% analysis$device_1up, ]
  #       # Also filter exposures
  #       if (nrow(thes) > 0 & !is.na(analysis$exp_device_1up) &
  #           analysis$exp_device_1up != "All" &
  #           nextdev == names(analysis$exp_device_1up)){
  #         thes <- thes[thes[[nextdev]] %in% analysis$exp_device_1up, ]
  #       }
  #     }
  #   }
  #   if ("isev" %in% atype){
  #     # Filter device-events to 1-level up event hierarchy
  #     nextev <- next_ev(names(analysis$event_level))
  #     if (nextev %in% names(this) & !is.na(analysis$event_1up) &
  #         nextev == names(analysis$event_1up)){
  #       nmiss <- sum(is.na(this[[nextev]]))
  #       if (nmiss > 0){
  #         warning(paste("Dropping", nmiss, "records with missing", nextev))
  #         this <- this[!is.na(this[[nextev]]), ]
  #       }
  #       this <- this[this[[nextev]] %in% analysis$event_1up]
  #     }
  #   }
  # }

  # Save the dataset if covariate or in-vivo analysis are defined
  # -------------------------------------------------------------
  if (analysis$invivo | analysis$covariate != "Data"){
    covar_data <- this
  } else covar_data <- NULL

  # Filter by the covariate level
  # -----------------------------
  this <- this[this$iscov, ]

  # Overall analysis date range
  # ---------------------------
  tr <- analysis$date_range_de

  # Loop through every deviceevent date period and count
  # ----------------------------------------------------
  ts_de <- data.frame()
  i <- tr[1]
  while (i <= tr[2]){
    j <- analysis$date_adder(i, 1)
    thist <- this[this$time >= i & this$time < j, ]
    if (dpa){
      # Populate contingency table
      if (nrow(thist) > 0){
        tbl2x2 <- list(t(stats::setNames(
          data.frame(table(factor(thist$isdev, levels=c(T, F)),
                           factor(thist$isev, levels=c(T, F))),
                     stringsAsFactors=T)[, 3],
          c("nA", "nC", "nB", "nD"))))
        # tbl2x2 <- list(t(stats::setNames(data.frame(table(
        #   thist[[names(analysis$device_level)]],
        #   thist[[names(analysis$event_level)]]))[, 3],
        #   c("nA", "nC", "nB", "nD"))))
        trow <- data.frame(time=i, tbl2x2, stringsAsFactors=T)
        tkey <- unique(as.character(thist[thist$isdev & thist$isev, ]$key))
        trow$ids <- list(ifelse(length(tkey) > 0, tkey, NA))
      } else{
        trow <- data.frame(time=i, nA=0, nB=0, nC=0, nD=0, stringsAsFactors=T)
        trow$ids <- list(c(NA))
      }
    } else{
      if (nrow(thist) > 0){
        trow <- data.frame(
          time=i,
          nA=sum(thist$isdev), stringsAsFactors=T)
        tkey <- unique(as.character(thist[thist$isdev, ]$key))
        trow$ids <- list(ifelse(length(tkey) > 0, tkey, NA))
      } else{
        trow <- data.frame(time=i, nA=0, stringsAsFactors=T)
        trow$ids <- list(c(NA))
      }
    }

    # if (length(atype) > 1){
    #   # 2-Level Analysis - Populate contingency table
    #   tbl2x2 <- list(t(stats::setNames(data.frame(table(thist[[atype[1]]],
    #                                                     thist[[atype[2]]]))[, 3],
    #                                    c("nA", "nC", "nB", "nD"))))
    #   trow <- data.frame(time=i, tbl2x2)
    #   trow$ids <- list(unique(
    #     as.character(thist[thist[[atype[1]]] == T &
    #                          thist[[atype[2]]] == T, ]$key)))
    # } else{
    #   # 1-Level Analysis - No contingency table
    #   trow <- data.frame(time=i, nA=table(thist[[atype[1]]])['TRUE'])
    #   trow$ids <- list(unique(as.character(thist[thist[[atype[1]]] == T, ]$key)))
    # }

    ts_de <- rbind(ts_de, trow)
    i <- j
  }
  rownames(ts_de) <- c()

  # Loop through every exposure date period and count
  # -------------------------------------------------
  if (!all(is.na(tr)) & nrow(thes) > 0){
    ts_e <- data.frame()
    i <- tr[1]
    while (i <= tr[2]){
      j <- analysis$date_adder(i, 1)
      thest <- thes[thes$time >= i & thes$time < j, ]
      trow <- data.frame(time=i, exposure=sum(thest$count), stringsAsFactors=T)
      tkey <- unique(as.character(thest$key))
      trow$ids_exposure <- list(ifelse(length(tkey) > 0, tkey, NA))
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
  # Determine if 1-Ups exist semantically
  dev_diff <- !is.na(analysis$device_1up) & names(analysis$device_level) !=
    names(analysis$device_1up)
  ev_diff <- !is.na(analysis$event_1up) & names(analysis$event_level) !=
    names(analysis$event_1up)
  # Construct the labels for the analysis levels and (if dpa) contingency table
  # Device level
  nhere <- analysis$device_level_source
  lab <- paste(nhere, analysis$device_level)
  notlab <- paste(nhere, "NOT", analysis$device_level)
  devlab <- c(lab, notlab)
  # 1 up device level
  if (dev_diff){
    lab1up <- paste("WITHIN", analysis$device_1up_source, analysis$device_1up)
    nhere <- paste(nhere, lab1up)
    lab <- paste(lab, lab1up)
    notlab <- paste(notlab, lab1up)
  }
  devlab <- c(lab, notlab, nhere)
  # Event level
  nhere <- analysis$event_level_source
  lab <- paste(nhere, analysis$event_level)
  notlab <- paste(nhere, "NOT", analysis$event_level)
  # 1 up event level
  if (ev_diff){
    lab1up <- paste("WITHIN", analysis$event_1up_source, analysis$event_1up)
    nhere <- paste(nhere, lab1up)
    lab <- paste(lab, lab1up)
    notlab <- paste(notlab, lab1up)
  }
  evlab <- c(lab, notlab, nhere)
  # Covariate
  covlab <- ifelse(is.na(analysis$covariate_level),
                   paste(nhere, "Missing"), # Covar NA
                   paste(nhere, analysis$covariate_level)) # Covar All, Covar level, Data All
  # Assign row and columns
  nRow <- devlab[3]
  nA <- devlab[1]
  nB <- devlab[1]
  nC <- devlab[2]
  nD <- devlab[2]
  nCol <- evlab[3]
  nA <- paste0(nA, ":", evlab[1])
  nB <- paste0(nB, ":", evlab[2])
  nC <- paste0(nC, ":", evlab[1])
  nD <- paste0(nD, ":", evlab[2])
  title <- paste(nRow, "by", nCol, "-", nA)

  # # Construct the labels for the analysis levels and (if dpa) contingency table
  # for (i in c(1:length(atype))){
  #   if (names(atype)[i] == "Covariate"){
  #     nhere <- analysis$covariate
  #     lab <- paste(nhere, analysis$covariate_level)
  #     notlab <- paste(nhere, "NOT", analysis$covariate_level)
  #   } else if (names(atype)[i] == "Device"){
  #     nhere <- analysis$device_level_source
  #     lab <- paste(nhere, analysis$device_level)
  #     notlab <- paste(nhere, "NOT", analysis$device_level)
  #     # 1 up device presence
  #     if (dev_diff){
  #       lab1up <- paste("WITHIN", analysis$device_1up_source,
  #                       analysis$device_1up)
  #       nhere <- paste(nhere, lab1up)
  #       lab <- paste(lab, lab1up)
  #       notlab <- paste(notlab, lab1up)
  #     }
  #   } else if (names(atype)[i] == "Event"){
  #     nhere <- analysis$event_level_source
  #     lab <- paste(nhere, analysis$event_level)
  #     notlab <- paste(nhere, "NOT", analysis$event_level)
  #     # 1 up event presence
  #     if (ev_diff){
  #       lab1up <- paste("WITHIN", analysis$event_1up_source,
  #                       analysis$event_1up)
  #       nhere <- paste(nhere, lab1up)
  #       lab <- paste(lab, lab1up)
  #       notlab <- paste(notlab, lab1up)
  #     }
  #   }
  #
  #   # Assign row and columns (only 2D analysis support so far)
  #
  #   ###########################
  #   # MUST FIX - ADD FACILITY FOR 3D ANALYSIS NAMING
  #   ###########################
  #
  #   if (i == 1){
  #     nRow <- nhere
  #     nA <- lab
  #     nB <- lab
  #     nC <- notlab
  #     nD <- notlab
  #     title <- nRow
  #   } else if (i == 2){
  #     nCol <- nhere
  #     nA <- paste0(nA, ":", lab)
  #     nB <- paste0(nB, ":", notlab)
  #     nC <- paste0(nC, ":", lab)
  #     nD <- paste0(nD, ":", notlab)
  #     title <- paste(title, "by", nCol)
  #   }
  # }

  # Save DPA details
  if (dpa){
    dpa_dtl <- list(nA=nA, nB=nB, nC=nC, nD=nD, nRow=nRow, nCol=nCol,
                    nABCD=title)
  } else{
    dpa_dtl <- NULL
  }

  #' ###########################
  #' # MUST FIX - ADD FACILITY FOR 3D ANALYSIS NAMING
  #' ###########################
  #' # I AM HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #' browser()
  #' # Remember that this is not filtered (just statuses)
  #' # While exposure thes is filtered already
  #' # devlvl
  #' # this$isdev
  #' # evlvl
  #' # this$isev
  #' # this$iscov
  #' #' Must consider 4 covariate possibilities
  #' #' 1. Data All
  #' #' 2. Covar All
  #' #' 3. Covar Level
  #' #' 4. Covar NA

  # Save the output class
  # ---------------------
  out <- structure(ts,
                   title=title,
                   analysis=analysis,
                   exposure=exposure,
                   dpa=dpa,
                   dpa_detail=dpa_dtl,
                   covar_data=covar_data)
  class(out) <- append("mds_ts", class(out))

  return(out)
}
