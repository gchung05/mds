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
  device_level="device_1",
  event_level=NULL,
  covariates=F,
  times_to_calc=NULL,
  prior=NULL
){
  # # What do I want next?
  # foo(deviceevents=testDE,
  #     exposure=NULL, # or specify mdpms.exposure object
  #     algorithms=NULL, # Default NULL just counts, otherwise several keywords
  #     date_level="month", # Default is month, otherwise day, quarter, semiannual, annual
  #     device_level=1, # Specify name
  #     event_level=NULL, # If null, does not aggregate at event level. Otherwise specify name.
  #     covariates=F, # Boolean stating whether to include analyses at each level of each covariate
  #     times_to_calc=NULL, # Null calculates all time, otherwise a number of units at the date level, reverse chron
  #     prior=NULL # A prior foo object. If specified, will only calculate new stuff (by time). If specified, times_to_calc is ignored.
  # )

  # Current possibles
  # -----------------
  # Algorithms where count is the default
  # algos <- c("count", "Shewhart-WE1", "EWMA-WE1", "PRR")
  # Date levels
  date_levs <- data.frame(day="", month="", quarter="",
                          semiannual="", annual="",
                          stringsAsFactors=F)

  # Check parameters
  input_param_checker(deviceevents, check_class="mdpms.deviceevents")
  input_param_checker(exposure, check_class="mdpms.exposure")
  # input_param_checker(algorithms, check_class="character",) # WILL NEED TO WORRY ABOUT THIS SOON
  input_param_checker(date_level, check_class="character",
                      check_names=date_levs, max_length=1)
  input_param_checker(device_level, check_class="character",
                      check_names=attributes(deviceevents)$device_hierarchy,
                      max_length=1)
  input_param_checker(event_level, check_class="character",
                      check_names=attributes(deviceevents)$event_hierarchy,
                      max_length)
  input_param_checker(times_to_calc, check_class="numeric", max_length=1)
  input_param_checker(prior, check_class="mdpms.analyses")

  # Testing
  deviceevents = testDE
  exposure = testEX
  algorithms = NULL
  date_level = "month"
  device_level="device_1"
  event_level=NULL
  covariates=F
  times_to_calc=NULL
  prior=NULL

  # MUST DO!!!!!!!!
  # create date holding variable that converts deviceevents$time to appropriate
  # date level. then use "date" variable

  # MUST DO
  # Check for prior. If no prior, filter set by times to calc


  # Devices - Enumerate (calculate the rollup level for the last loop)
  # ------------------------------------------------------------------
  uniq_devs <- c(unique(as.character(deviceevents[[device_level]])), "All")
  i <- 1
  while (i <= length(uniq_devs)){
    devDE <- deviceevents
    # Filter for the current device
    # device is a holding variable
    if (i == length(uniq_devs)){
      devDE$device <- "All"
    } else{
      devDE <- devDE[devDE[[device_level]] == uniq_devs[i], ]
      devDE$device <- devDE[[device_level]]
    }

    # Events - Enumerate (calculate the rollup level for the last loop)
    # -----------------------------------------------------------------
    if (is.null(event_level)){
      uniq_evts <- c("All")
    } else{
      uniq_evts <- c(unique(devDE[[event_level]]), "All")
    }
    j <- 1
    while (j <= length(uniq_evts)){
      # Filter for the current event
      # event is a holding variable
      if (j == length(uniq_evts)){
        devDE$event <- "All"
      } else{
        devDE <- devDE[devDE[[event_level]] == uniq_evts[j], ]
        devDE$event <- devDE[[event_level]]
      }

      # Covariates - Enumerate (calculate the rollup level for the last loop)
      # ---------------------------------------------------------------------
      if (!covariates){
        uniq_covs <- list("Data"="All")
      } else{
        uniq_covs <- lapply(attributes(devDE)$covariates, function(x){
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
          # Calculate the acceptable timeframe for analysis
          # Save the planned algorithms to use

          # Exposure Case
          # -------------
          #' How to handle mismatched levels in exposure?
          #' Assume for any specific level of device, event, covariate, that
          #' exposure must be matched to that specific granularity
          #' However, if any of device, event, covariate is of the value 'All',
          #' then exposure levels are ignored for that variable. In these cases,
          #' all exposures are used to calculate the date windows

          # I am here!
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
