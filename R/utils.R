#' Check input parameters
#'
#' Verifies correct class and, optionally, verifies existence in a data frame
#'
#' @param x Single object name, character, or vector of characters
#' @param check_class Name of correct class in character format
#' @param check_names Optional: Data frame object
#'
#' @return Stop error, if an error is found. Else nothing.
input_param_checker <- function(
  x,
  check_class=NULL,
  check_names=NULL
){
  if (length(check_class) > 0){
    if (length(check_names) > 0){
      if (class(x) == "character"){
        this_class <- lapply(check_names[x], class)
      } else this_class <- list(class(x))
    } else this_class <- list(class(x))
    if (!all(sapply(this_class, function(x) check_class %in% x))){
      stop(paste0(ifelse(class(x) == "character", paste(x, collapse=" "), quote(x)),
                  " must be of class '", check_class, "'"))
    }
  }
  if (length(check_names) > 0){
    if(!all(x %in% names(check_names))){
      stop(paste0(x, " names not found in ", check_names))
    }
  }
}

temp <- readxl::read_excel("E:/Projects/paradoxEgression/data/raw/VOC_CodesCombined_PI_Data_November2015_R2.xlsx")
names(temp) <- make.names(names(temp))
temp$Date <- as.Date(temp$Date)

device_event_class(temp,
                   key="Product.Issue.Number",
                   time="Date",
                   device_hierarchy=c("Functional.Family", "Product.Family"),
                   event_hierarchy=c("VOC_New"))
