#' Assess & Save MD-PMS Analyses Definitions
#'
#' Define analyses based on an MD-PMS device-event data frame and (optionally)
#' an MD-PMS exposure data frame.
#'
#' @param deviceevents Object of class \code{mdpms.deviceevents}
#' @param exposure Optional object of class \code{mdpms.exposure}. See details
#' for how exposure analyses definitions are handled.
#'
#' Default: \code{NULL} will not consider inclusion of exposure.
#' @param algorithms To write
#' @param date_level String value for the primary date unit to analyze by. Can
#' be either \code{'months'} or \code{'days'}.
#'
#' Default: \code{'months'}
#' @param date_level_n Numeric value indicating the number of \code{date_level}s
#' to analyze by.
#'
#' Default: \code{1}
#'
#' Example: \code{date_level='months'} and \code{date_level_n=3} indicates
#' analysis on a quarterly level.
#' @param device_level String value indicating the source device variable name
#' to analyze by.
#'
#' Example: If the \code{deviceevents} variable column is \code{device_1} where
#' the source variable name for \code{device_1} is \code{'Device Code'}, specify
#' \code{device_level='Device Code'}.
#' @param event_level String value indicating the event variable name in
#' \code{deviceevents} to analyze by. Note that \code{event_level} is not
#' matched to \code{exposure}.
#'
#' Default: \code{NULL} will not analyze by event.
#'
#' Example: \code{"event_1"}
#' @param covariates Character vector specifying names of covariates to also
#' define analyses for. Acceptable names are covariate variable names from
#' \code{deviceevents}. Analyses will be defined for each unique level of each
#' covariate. \code{"_none_"} specifies no covariates, while \code{"_all_"}
#' specifies all covariates from \code{deviceevents}.
#'
#' Default: \code{"_none_"} specifies no covariates.
#'
#' Example: \code{"Country"}
#' @param times_to_calc Integer value indicating the number of date units
#' counting backwards from the latest date to define analyses for. If
#' \code{prior} is specified, \code{times_to_calc} will be ignored.
#'
#' Default: \code{NULL} will define analyses across all available time.
#'
#' Example 1: \code{times_to_calc=12} with \code{date_level="months"} and
#' \code{date_level_n=1} defines analyses for the last year by month.
#'
#' Example 2: \code{times_to_calc=8} with \code{date_level="months"} and
#' \code{date_level_n=3} defines analyses for the 2 years by quarter.
#' @param prior An object of class \code{mdpms.define_analyses} to NEED TO WRITE.
#' @return A list of defined analyses of class \code{mdpms.define_analyses}.
#' Each list item, indexed by a numeric key, defines a set of analyses for a
#' unique combination of device, event, and covariate level. Each list item is
#' of the class \code{mdpms.define_analysis}.
#' Attributes are as follows:
#' \describe{
#'   \item{date_level}{Defined value for \code{date_level}}
#'   \item{date_level_n}{Defined value for \code{date_level_n}}
#'   \item{device_level}{Defined value for \code{device_level}}
#'   \item{event_level}{Defined value for \code{event_level}}
#'   \item{times_to_calc}{Defined value for \code{times_to_calc}}
#'   \item{prior_used}{Boolean for whether \code{prior} was specified.}
#'   \item{timestamp}{System time when the analyses were defined.}
#' }
#' @details The analyses definitions will always include rollup levels for each
#' of \code{device_level}, \code{event_level} (if specified), and
#' \code{covariates}. These rollup analyses will be indicated by the keyword
#' 'All', while the rollup of all covariates will be called 'Data'.
#'
#' If \code{exposure} is specified, any available \code{match_levels} will be
#' used to calculate the appropriate timeframe for analyses. The exception are
#' the special rollup analyses (see prior paragraph).
#' @examples
#' ## Need to write!
#'
#' @export
define_analyses <- function(
  deviceevents,
  exposure=NULL,
  algorithms=NULL,
  date_level="month",
  device_level,
  event_level=NULL,
  covariates="_none_",
  times_to_calc=NULL,
  prior=NULL
){
  # Current possibles
  # -----------------
  # Algorithms where count is the default
  # algos <- c("count", "Shewhart-WE1", "EWMA-WE1", "PRR")
  # Covariates
  if (covariates == "_none_"){
    covariates <- NULL
  } else if (covariates == "_all_"){
    covariates <- names(attributes(deviceevents)$covariates)
  }

  # Check parameters
  # ----------------
  input_param_checker(deviceevents, check_class="mdpms.deviceevents")
  input_param_checker(exposure, check_class="mdpms.exposure")
  # input_param_checker(algorithms, check_class="character",) # WILL NEED TO WORRY ABOUT THIS SOON
  input_param_checker(date_level_n, check_class="numeric", max_length=1)
  input_param_checker(device_level, check_class="character",
                      check_names=char_to_df(
                        attributes(deviceevents)$device_hierarchy),
                      max_length=1)
  input_param_checker(event_level, check_class="character",
                      check_names=char_to_df(
                        names(attributes(deviceevents)$event_hierarchy)),
                      max_length=1)
  input_param_checker(covariates, check_class="character",
                      check_names=char_to_df(
                        names(attributes(deviceevents)$covariates)))
  input_param_checker(times_to_calc, check_class="numeric", max_length=1)
  input_param_checker(prior, check_class="mdpms.analyses")

  # Filter deviceevents and exposure by times_to_calc if prior is NULL
  # ------------------------------------------------------------------
  if (is.null(prior) & !is.null(times_to_calc)){
    # Get the latest date
    latest_date <- max(max(deviceevents$time),
                       dplyr::if_else(is.null(exposure), as.Date("1900-01-01"),
                                      max(exposure$time)))
    latest_date <- convert_date(latest_date, date_level, date_level_n)
    # Calculate the lower cutoff date
    cutoff_date <- attributes(latest_date)$adder(latest_date, -times_to_calc)
    # Filter
    deviceevents <- deviceevents[deviceevents$time >= cutoff_date, ]
    if (!is.null(exposure)){
      exposure <- exposure[exposure$time >= cutoff_date, ]
    }
  }

  # Set analysis output & analysis index (index will become primary key)
  out <- list(); z <- 1

  # Devices - Enumerate (calculate the rollup level for the last loop)
  # ------------------------------------------------------------------
  dev_lvl <- names(which(attributes(deviceevents)$device_hierarchy == device_level))
  uniq_devs <- c(unique(as.character(deviceevents[[dev_lvl]])), "All")
  i <- 1
  while (i <= length(uniq_devs)){
    devDE <- deviceevents
    # Filter for the current device
    # device is a holding variable
    if (i == length(uniq_devs)){
      devDE$device <- "All"
    } else{
      devDE <- devDE[devDE[[dev_lvl]] == uniq_devs[i], ]
      devDE$device <- devDE[[dev_lvl]]
    }

    # Events - Enumerate (calculate the rollup level for the last loop)
    # -----------------------------------------------------------------
    ev_lvl <- names(which(attributes(deviceevents)$event_hierarchy == event_level))
    if (is.null(ev_lvl)){ # Set rollup level
      uniq_evts <- c("All")
    } else{
      uniq_evts <- c(unique(devDE[[ev_lvl]]), "All")
    }
    j <- 1
    while (j <= length(uniq_evts)){
      # Filter for the current event
      # event is a holding variable
      if (j == length(uniq_evts)){ # Set rollup level
        devDE$event <- "All"
      } else{
        devDE <- devDE[devDE[[ev_lvl]] == uniq_evts[j], ]
        devDE$event <- devDE[[ev_lvl]]
      }

      # Covariates - Enumerate (calculate the rollup level for the last loop)
      # ---------------------------------------------------------------------
      # Enumerate each level of each covariate
      if (is.null(covariates)){
        uniq_covs <- list("Data"="All")
      } else{
        uniq_covs <- lapply(covariates, function(x){
          this <- unique(as.character(devDE[[x]]))
          this <- this[!is.na(this)]
        })
        names(uniq_covs) <- covariates
        uniq_covs$Data <- "All" # Set rollup level
      }

      # Save analysis instructions for each level of device, event, covariate
      # ---------------------------------------------------------------------
      for (k in names(uniq_covs)){
        for (l in uniq_covs[[k]]){
          # Filter for the current covariate level
          if (paste(k, l) != "Data All"){
            devDE <- devDE[devDE[[k]] == l, ]
          }

          # Non-Exposure Case
          # -----------------
          # Establish date range
          dt_range <- convert_date(range(devDE$time, na.rm=T),
                               date_level, date_level_n)
          names(dt_range) <- c("start", "end")
          # Build list of instructions
          this <- list(device_level,
                       setNames(devDE$device[1], dev_lvl),
                       setNames(devDE$event[1], ifelse(is.null(ev_lvl), NA,
                                                       ev_lvl)),
                       k, l, dt_range, algorithms)
          names(this) <- c("device_level_source",
                           "device_level", "event_level",
                           "covariate", "covariate_level",
                           "date_range_de", "algorithms")

          # Exposure Case
          # -------------
          if (is.null(exposure)){ thes <- data.frame() } else thes <- exposure
          # Filter by device
          if (nrow(thes) > 0 &
              this$device_level != "All" &
              this$device_level_source %in% attributes(exposure)$device_hierarchy){
            dev_level_e <- names(which(attributes(exposure)$device_hierarchy ==
                                         this$device_level_source))
            thes <- thes[thes[[dev_level_e]] == as.character(this$device_level), ]
          }
          # Filter by event
          # <A possible future feature, if requested.>
          # Filter for the current covariate level
          if (nrow(thes) > 0 &
              this$covariate != "Data" &
              this$covariate %in% attributes(exposure)$match_levels){
            thes <- thes[thes[[this$covariate]] ==
                           as.character(this$covariate_level), ]
          }
          # Establish exposure date range, if exposure data exists
          if (nrow(thes) > 0){
            dt_range <- convert_date(range(thes$time, na.rm=T),
                                     date_level, date_level_n)
            names(dt_range) <- c("start", "end")
            this$date_range_exposure <- dt_range
          } else this$date_range_exposure <- c(as.Date(NA), as.Date(NA))
          # Establish date range if exposure is to be used in analysis
          dt_range <- c(
            max(c(this$date_range[1], this$date_range_exposure[1]), na.rm=T),
            min(c(this$date_range[2], this$date_range_exposure[2]), na.rm=T))
          dt_range <- convert_date(dt_range, date_level, date_level_n)
          this$date_range_de_exp <- dt_range

          # Finally, save the analysis
          # --------------------------
          class(this) <- append(class(this), "mdpms.define_analysis")
          out[[z]] <- this
          z <- z + 1
        }
      }
      j <- j + 1
    }
    i <- i + 1
  }

  # Save the output class
  # ---------------------
  out <- structure(out,
                   date_level=date_level,
                   date_level_n=date_level_n,
                   device_level=device_level,
                   event_level=event_level,
                   times_to_calc=times_to_calc,
                   prior_used=!is.null(prior),
                   timestamp=Sys.time())
  class(out) <- append(class(out), "mdpms.define_analyses")

  return(out)
}

# # Testing
# deviceevents = testDE
# exposure = testEX
# algorithms = NULL
# date_level = "months"
# date_level_n=1
# device_level="Functional Family"
# event_level=NULL
# covariates="_none_"
# times_to_calc=NULL
# prior=NULL
