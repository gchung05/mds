


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
  event_hierarchy
){
  # Check parameters
  input_param_checker(data_frame, check_class="data.frame")
  input_param_checker(key, check_class="character", check_names=data_frame)
  input_param_checker(time, check_class="Date", check_names=data_frame)
  input_param_checker(device_hierarchy, check_class="character",
                      check_names=data_frame)
  input_param_checker(event_hierarchy, check_class="character",
                      check_names=data_frame)

  print("Hello, world!")

}
