#' Plot MD-PMS Time Series
#'
#' Quickly visualizes an MD-PMS times series of class \code{mds_ts}.
#'
#' @param x An object of class \code{mds_ts}.
#' @param mode Series to plot. Valid values are: \code{'nA'}, \code{'nB'},
#' \code{'nC'}, \code{'nD'}, \code{'exposure'}, \code{'rate'}. \code{'rate'} is
#' simply \code{'nA' / 'exposure'}. See details for more.
#'
#' Default: \code{'nA'}
#'
#' @param xlab x-axis label
#'
#' #' Default: \code{'Time'}
#'
#' @param ylab y-axis label
#'
#' Default: \code{'Count'}
#'
#' @param main Plot title
#'
#' Default: \code{NULL} infers the title from \code{x} and \code{mode}.
#'
#' @param ... Further arguments to pass onto \code{plot()} generic.
#'
#' @details \code{mode} values defined as follows. Note: The following
#' definitions use a device-event pair as a working example, however it may also
#' be a covariate-device pair.
#' \describe{
#'   \item{'nA'}{Counts of the device-event pair.}
#'   \item{'nB'}{Counts of the device for all other events.}
#'   \item{'nC'}{Counts of all other devices for the event.}
#'   \item{'nD'}{Counts of all other devices for all other events.}
#'   \item{'exposure'}{Counts of exposure for the device-event pair.}
#'   \item{'rate'}{A crude rate, calculated as the device-event counts pair
#'   divided by the exposure counts.}
#' }
#' @export
plot.mds_ts <- function(
  x,
  mode='nA',
  xlab='Time',
  ylab='Count',
  main=NULL,
  ...
){
  obj <- data.frame(x=x$time)
  if (mode == 'nA'){
    obj$y <- x[[mode]]
    # Set the default title
    if (is.null(main)){
      main <- paste(attributes(x)$nLabels$nA, collapse=" & ")
    }
  } else if (mode %in% c('nB', 'nC', 'nD', 'exposure')){
    if (!mode %in% names(x)){
      stop(paste('The analysis of x does not contain', mode))
    }
    obj$y <- x[[mode]]
    # Set the default title
    if (is.null(main)){
      if (mode != 'exposure'){
        if (mode == 'nB'){
          main <- paste(attributes(x)$nLabels$rows, 'excluding',
                        attributes(x)$nLabels$nA[1], '&',
                        attributes(x)$nLabels$nA[2])
        } else if (mode == 'nC'){
          main <- paste(attributes(x)$nLabels$nA[1], '&',
                        attributes(x)$nLabels$cols, 'excluding',
                        attributes(x)$nLabels$nA[2])
        } else if (mode == 'nD'){
          main <- paste(attributes(x)$nLabels$rows, 'excluding',
                        attributes(x)$nLabels$nA[1], '&',
                        attributes(x)$nLabels$cols, 'excluding',
                        attributes(x)$nLabels$nA[2])
        }
      } else main <- 'Exposure'
    }
  } else if (mode == 'rate'){
    if (!'exposure' %in% names(x)){
      stop(paste('x requires exposure to calculate rate'))
    }
    obj$y <- x$nA / x$exposure
    if (ylab == 'Count') ylab <- 'Rate'
    # Set the default title
    if (is.null(main)){
      main <- paste(paste(attributes(x)$nLabels$nA, collapse=" & "),
                    '/ Exposure')
    }
  } else{
    stop(paste(mode, 'is not a valid mode'))
  }
  graphics::plot.default(obj$x, obj$y, xlab=xlab, ylab=ylab, main=main, ...)
}
