#' MD-PMS Exposure Data Frame
#'
#' Converts a data frame into a MD-PMS Exposure data frame.
#'
#' @param data_frame The input data frame requiring components specified in the
#' remaining arguments.
#'
#' @param key Character name of (uniquely identifying) primary key variable in
#' \code{data_frame}. Class must be character or numeric.
#'
#' Example: \code{"key_ID"}
#'
#' Default: \code{NULL} will create a key variable.
#'
#' @param time Character name of date variable in \code{data_frame}. Class must
#' be Date, POSIXt, or character.
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
#' Example: \code{c("Family", "Device", "ProductCode")}
#'
#' Default: \code{NULL} will not include any event hierarchy.
#'
#' @param match_levels Vector of character variable names in \code{data_frame}
#' representing additional grouping factors for exposure. Specified variables
#' will be implicitly matched to equivalently named variables contained in the
#' \code{mds_de} object class.
#'
#' Example: \code{c("Country", "Region")}
#'
#' Default: \code{NULL} will not include any additional grouping factors.
#'
#' @param count Character name of exposure count variable in \code{data_frame}.
#' Class must be numeric.
#'
#' Example: \code{"Units Sold"}
#'
#' Default: \code{NULL} will assume each row represents one exposure.
#'
#' @return A standardized MD-PMS data frame of class \code{mds_e}.
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
#'   \item{match_levels}{Vector of variable names for grouping factors}
#'   \item{count}{Original variable name for \code{count}}
#' }
#'
#' @examples
#' # A barebones dataset
#' ex <- exposure(sales, "sales_month", "device_name")
#' # With more variables and variable types
#' ex <- exposure(
#'   data_frame=sales,
#'   time="sales_month",
#'   device_hierarchy="device_name",
#'   match_levels="region",
#'   count="sales_volume")
#'
#' @export
exposure <- function(
  data_frame,
  time,
  device_hierarchy,
  event_hierarchy=NULL,
  key=NULL,
  match_levels=NULL,
  count=NULL
){
  # Check parameters
  # ----------------
  input_param_checker(data_frame, check_class="data.frame")
  input_param_checker(time, check_class=c("character", "POSIXt", "Date"),
                      check_names=data_frame)
  input_param_checker(device_hierarchy, check_class="character",
                      check_names=data_frame)
  input_param_checker(event_hierarchy, check_class="character",
                      check_names=data_frame)
  input_param_checker(key, check_class=c("character", "numeric"),
                      check_names=data_frame)
  input_param_checker(match_levels, check_class="factor",
                      check_names=data_frame)
  input_param_checker(count, check_class=c("numeric", "integer"),
                      check_names=data_frame)

  # Address each variable
  # ---------------------
  # Time
  v_time <- as.Date(parsedate::parse_date(data_frame[[time]]))
  # Device Hierarchy
  names(device_hierarchy) <- paste0("device_", c(1:length(device_hierarchy)))
  v_dev <- list()
  for (i in c(1:length(device_hierarchy))){
    v_dev[[names(device_hierarchy)[i]]] <- data_frame[[device_hierarchy[i]]]
  }
  # Event Hierarchy
  if (!is.null(event_hierarchy)){
    names(event_hierarchy) <- paste0("event_", c(1:length(event_hierarchy)))
    v_ev <- list()
    for (i in c(1:length(event_hierarchy))){
      v_ev[[names(event_hierarchy)[i]]] <- data_frame[[event_hierarchy[i]]]
    }
  }
  # Key
  if (is.null(key)){
    v_key <- as.character(c(1:nrow(data_frame)))
  } else{
    v_key <- as.character(data_frame[[key]])
  }
  # Match Levels
  if (!is.null(match_levels)){
    v_ml <- list()
    # Must drop any covariates that are not factors
    b <- unlist(lapply(data_frame[, as.character(match_levels), drop=F], is.factor))
    if (any(!b)){
      bad_covs <- names(b)[!b]
      warning(paste0("Non-factor specified in match_levels dropped:",
                     paste(bad_covs, collapse=", ")))
      match_levels <- match_levels[!as.character(match_levels) %in% bad_covs]
    }
    if (length(match_levels) > 0){
      for (i in c(1:length(match_levels))){
        v_ml[[match_levels[i]]] <- data_frame[[match_levels[i]]]
      }
    } else stop("Variable(s) specified by match_levels must be factor(s).")
  }
  # Count
  if (is.null(count)){
    ct <- c("count"=NA)
    v_ct <- rep(1, nrow(data_frame))
  } else{
    ct <- c("count"=count)
    v_ct <- data_frame[[count]]
  }

  # Assemble data frame
  # -------------------
  dataset <- cbind.data.frame(
    data.frame(key=v_key, time=v_time, count=v_ct, stringsAsFactors=F),
    data.frame(v_dev))
  if (!is.null(event_hierarchy)){
    dataset <- cbind.data.frame(dataset, data.frame(v_ev))
  }
  if (!is.null(match_levels)){
    dataset <- cbind.data.frame(dataset, data.frame(v_ml))
  }

  # Cleanup
  # -------
  # Deduplicate data frame
  uds <- unique(dataset)
  if (nrow(uds) < nrow(dataset)){
    warning(paste("Dropping", nrow(dataset) - nrow(uds), "duplicate rows."))
    dataset <- uds
  }
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
  if (!is.null(event_hierarchy)){
    for (i in names(v_ev)){
      if (sum(is.na(dataset[[i]])) > 0){
        warning(paste("Dropping", sum(is.na(dataset[[i]])),
                      "rows with missing", i, "event_hierarchy."))
        dataset <- dataset[!is.na(dataset[[i]]), ]
      }
    }
  }
  # Missing match levels
  if (!is.null(match_levels)){
    for (i in names(v_ml)){
      if (sum(is.na(dataset[[i]])) > 0){
        warning(paste("Dropping", sum(is.na(dataset[[i]])),
                      "rows with missing", i, "match levels"))
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
                   match_levels=match_levels,
                   count=count)
  class(out) <- append("mds_e", class(out))

  return(out)
}
