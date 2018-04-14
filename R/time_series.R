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
  inlist
){
  # Check parameters
  # ----------------
  input_param_checker(data_frame, check_class="data.frame")
  input_param_checker(key, check_class=c("character", "numeric"),
                      check_names=data_frame)
  input_param_checker(descriptors, check_class="character",
                      check_names=data_frame, exclusions="_all_")
  input_param_checker(device_level, check_class="character",
                      check_names=char_to_df(
                        attributes(deviceevents)$device_hierarchy),
                      max_length=1)

  # Address each variable
  # ---------------------
  # Key
  if (is.null(key)){
    v_key <- as.character(c(1:nrow(data_frame)))
  } else{
    v_key <- as.character(data_frame[[key]])
  }

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
