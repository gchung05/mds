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
  device_level,
  event_level=NULL,
  covariates=F,
  times_to_calc=NULL,
  prior=NULL
){
  # # What do I want next?
  # foo(deviceevents=testDE,
  #     exposure=NULL, # or specify mdpms.exposure object
  #     algorithms=NULL, # Default NULL just counts, otherwise several keywords
  #     date_level="months", # Default is months, can also be days.
  #     date_level_n=1, # Default is 1 (1 month).
  #     device_level, # Specify name of device variable name using the source name
  #     event_level=NULL, # If null, does not aggregate at event level. Otherwise specify name.
  #     covariates="_none_", # Vector specifying names of covariates to also define analyses for. _all_ indicates all, _none_ is none.
  #     times_to_calc=NULL, # Null calculates all time, otherwise a number of units at the date level, reverse chron
  #     prior=NULL # A prior foo object. If specified, will only calculate new stuff (by time). If specified, times_to_calc is ignored.
  # )

  # Testing
  deviceevents = testDE
  exposure = testEX
  algorithms = NULL
  date_level = "months"
  date_level_n=1
  device_level="Functional Family"
  event_level=NULL
  covariates="_none_"
  times_to_calc=NULL
  prior=NULL

  # Current possibles
  # -----------------
  # Algorithms where count is the default
  # algos <- c("count", "Shewhart-WE1", "EWMA-WE1", "PRR")
  # Covariates
  if (covariates == "_none_"){
    covariates <- NULL
  } else if (covariates == "_all_"){
    covariates <- attributes(deviceevents)$covariates
  }

  # Check parameters
  input_param_checker(deviceevents, check_class="mdpms.deviceevents")
  input_param_checker(exposure, check_class="mdpms.exposure")
  # input_param_checker(algorithms, check_class="character",) # WILL NEED TO WORRY ABOUT THIS SOON
  input_param_checker(date_level_n, check_class="numeric", max_length=1)
  input_param_checker(device_level, check_class="character",
                      check_names=char_to_df(attributes(deviceevents)$device_hierarchy),
                      max_length=1)
  input_param_checker(event_level, check_class="character",
                      check_names=char_to_df(attributes(deviceevents)$event_hierarchy),
                      max_length=1)
  input_param_checker(covariates, check_class="character",
                      check_names=attributes(deviceevents)$covariates)
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
    if (is.null(ev_lvl)){
      uniq_evts <- c("All")
    } else{
      uniq_evts <- c(unique(devDE[[ev_lvl]]), "All")
    }
    j <- 1
    while (j <= length(uniq_evts)){
      # Filter for the current event
      # event is a holding variable
      if (j == length(uniq_evts)){
        devDE$event <- "All"
      } else{
        devDE <- devDE[devDE[[ev_lvl]] == uniq_evts[j], ]
        devDE$event <- devDE[[ev_lvl]]
      }

      # Covariates - Enumerate (calculate the rollup level for the last loop)
      # ---------------------------------------------------------------------
      if (is.null(covariates)){
        uniq_covs <- list("Data"="All")
      } else{
        uniq_covs <- lapply(covariates, function(x){
          unique(as.character(devDE[[x]]))
        })
        uniq_covs$Data <- "All"
      }
      for (k in names(uniq_covs)){
        for (l in uniq_covs[[k]]){
          # Filter for the current covariate level
          if (paste(k, l) != "Data All"){
            devDE <- devDE[devDE[[k]] == l, ]
          }

          # Non-Exposure Case
          # -----------------
          dt_range <- convert_date(range(devDE$time, na.rm=T),
                               date_level, date_level_n)
          names(dt_range) <- c("start", "end")

          this <- list(setNames(devDE$device[1], dev_lvl),
                       setNames(devDE$event[1], ifelse(is.null(ev_lvl), NA,
                                                       ev_lvl)),
                       k, l, dt_range, algorithms)
          names(this) <- c("device_level", "event_level",
                           "covariate", "covariate_level",
                           "date_info", "algorithms")

          # Exposure Case
          # -------------
          thes <- exposure
          # Filter by device
          if (device_level %in% attributes(exposure)$device_hierarchy){
            dev_level_e <- names(which(attributes(exposure)$device_hierarchy ==
                                         device_level))
            if (devDE$device[1] != "All"){
              thes <- thes[thes[[dev_level_e]] == devDE$device[1], ]
            }
          }
          # Filter by event
          if (event_level %in% attributes(exposure)$event_hierarchy){
            ev_level_e <- names(which(attributes(exposure)$event_hierarchy ==
                                        event_level))
            if (devDE$event[1] != "All"){
              thes <- thes[thes[[ev_level_e]] == devDE$event[1], ]
            }
          }
          # Filter by covariate

          # I am here!


          # Calculate the date range based on exposure availability

          #' How to handle mismatched levels in exposure?
          #' Assume for any specific level of device, event, covariate, that
          #' exposure must be matched to that specific granularity
          #' However, if any of device, event, covariate is of the value 'All',
          #' then exposure levels are ignored for that variable. In these cases,
          #' all exposures are used to calculate the date windows

        }
      }

      j <- j + 1
    }
    i <- i + 1
  }



  #######################################

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
