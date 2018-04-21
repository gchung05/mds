#' Plot MD-PMS Time Series
#'
#' Quickly visualizes an MD-PMS times series of class \code{mds_ts}.
#'
#' @param tsobj An object of class \code{mds_ts}.
#' @param mode Series to plot. Valid values are: \code{'nA'}, \code{'nB'},
#' \code{'nC'}, \code{'nD'}, \code{'exposure'}, \code{'rate'}. \code{'rate'} is
#' simply \code{'nA' / 'exposure'}. See details for more.
#'
#' Default: \code{'nA'}
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
  tsobj,
  mode='nA',
  xlab='Time',
  ylab='Count',
  main=NULL,
  ...
){
  obj <- data.frame(x=tsobj$time)
  if (mode == 'nA'){
    obj$y <- tsobj[[mode]]
    # Set the default title
    if (is.null(main)){
      main <- paste(attributes(tsobj)$nLabels$nA, collapse=" & ")
    }
  } else if (mode %in% c('nB', 'nC', 'nD', 'exposure')){
    if (!mode %in% names(tsobj)){
      stop(paste('The analysis of tsobj does not contain', mode))
    }
    obj$y <- tsobj[[mode]]
    # Set the default title
    if (is.null(main)){
      if (mode != 'exposure'){
        if (mode == 'nB'){
          main <- paste(attributes(tsobj)$nLabels$rows, 'excluding',
                        attributes(tsobj)$nLabels$nA[1], '&',
                        attributes(tsobj)$nLabels$nA[2])
        } else if (mode == 'nC'){
          main <- paste(attributes(tsobj)$nLabels$nA[1], '&',
                        attributes(tsobj)$nLabels$cols, 'excluding',
                        attributes(tsobj)$nLabels$nA[2])
        } else if (mode == 'nD'){
          main <- paste(attributes(tsobj)$nLabels$rows, 'excluding',
                        attributes(tsobj)$nLabels$nA[1], '&',
                        attributes(tsobj)$nLabels$cols, 'excluding',
                        attributes(tsobj)$nLabels$nA[2])
        }
      } else main <- 'Exposure'
    }
  } else if (mode == 'rate'){
    if (!'exposure' %in% names(tsobj)){
      stop(paste('tsobj requires exposure to calculate rate'))
    }
    obj$y <- tsobj$nA / tsobj$exposure
    if (ylab == 'Count') ylab <- 'Rate'
    # Set the default title
    if (is.null(main)){
      main <- paste(paste(attributes(tsobj)$nLabels$nA, collapse=" & "),
                    '/ Exposure')
    }
  } else{
    stop(paste(mode, 'is not a valid mode'))
  }
  plot.default(obj$x, obj$y, xlab=xlab, ylab=ylab, main=main, ...)
}
