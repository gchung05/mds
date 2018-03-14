#' Title
#'
#' Short description
#'
#' @param data_frame The input described
#'
#' Long description
#' blah blah blah
#'
#' @return The object of something.
#'
#' @examples
#' helloworld()
#'
#' @export
device_event_class <- function(
  data_frame,
  key,
  time,
  device_hierarchy,
  event_hierarchy,
  covariates=NULL
){
  # Check parameters
  input_param_checker(data_frame, check_class="data.frame")
  input_param_checker(key, check_class="character", check_names=data_frame)
  input_param_checker(time, check_class="Date", check_names=data_frame)
  input_param_checker(device_hierarchy, check_class="character",
                      check_names=data_frame)
  input_param_checker(event_hierarchy, check_class="character",
                      check_names=data_frame)
  # Key
  v_key <- as.character(data_frame[[key]])
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
  key_vars <- c(key, time, device_hierarchy, event_hierarchy)
  if (is.null(covariates)){
    covs <- NULL
  } else if (covariates == "_all_"){
    covs <- names(data.frame)[which(!names(data.frame) %in% key_vars)]
    names(covs) <- make.names(covs)
  } else{
    covs <- covariates
    names(covs) <- make.names(covs)
  }
  if (!is.null(covs)){
    v_cov <- list()
    for (i in c(1:length(covs))){
      v_cov[[names(covs)[i]]] <- data_frame[[covs[i]]]
    }
  }
  # Assemble data frame
  dataset <- cbind.data.frame(
    data.frame(key=v_key, time=v_time),
    data.frame(v_dev),
    data.frame(v_ev))
  if (!is.null(covs)){
    dataset <- cbind.data.frame(dataset, data.frame(v_cov))
  }
  # Save the output class
  out <- structure(dataset,
                   key=key,
                   time=time,
                   device_hierarchy=device_hierarchy,
                   event_hierarchy=event_hierarchy,
                   covariates=covs)
  class(out) <- append(class(out), "mdpmsdata")

  print("Hello, world!")

}
