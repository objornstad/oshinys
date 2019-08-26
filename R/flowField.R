#' Flowfield
#'
#' Plots the flow or velocity field for a one- or two-dimensional autonomous ODE
#' system.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required format of these functions
#' can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param xlim In the case of a two-dimensional system, this sets the limits of
#' the first dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one-dimensional system, this sets the
#' limits of the independent variable in which these line segments should be
#' plotted. Should be a \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' of \code{\link[base]{length}} two.
#' @param ylim In the case of a two-dimensional system this sets the limits of
#' the second dependent variable in which gradient reflecting line segments
#' should be plotted. In the case of a one-dimensional system, this sets the
#' limits of the dependent variable in which these line segments should be
#' plotted. Should be a \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' of \code{\link[base]{length}} two.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{NULL}.
#' @param points Sets the density of the line segments to be plotted;
#' \code{points} segments will be plotted in the x and y directions. Fine tuning
#' here, by shifting \code{points} up and down, allows for the creation of more
#' aesthetically pleasing plots. Defaults to \code{11}.
#' @param system Set to either \code{"one.dim"} or \code{"two.dim"} to indicate
#' the type of system being analysed. Defaults to \code{"two.dim"}.
#' @param col Sets the colour of the plotted line segments. Should be a
#' \code{\link[base]{character}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} one. Will be reset accordingly if it is of the
#' wrong \code{\link[base]{length}}. Defaults to \code{"gray"}.
#' @param arrow.type Sets the type of line segments plotted. If set to
#' \code{"proportional"} the \code{\link[base]{length}} of the line segments
#' reflects the magnitude of the derivative. If set to \code{"equal"} the line
#' segments take equal lengths, simply reflecting the gradient of the
#' derivative(s). Defaults to \code{"equal"}.
#' @param arrow.head Sets the length of the arrow heads. Passed to
#' \code{\link[graphics]{arrows}}. Defaults to \code{0.05}.
#' @param frac Sets the fraction of the theoretical maximum length line
#' segments can take without overlapping, that they can actually attain. In
#' practice, \code{frac} can be set to greater than 1 without line segments
#' overlapping. Fine tuning here assists the creation of aesthetically pleasing
#' plots. Defaults to \code{1}.
#' @param add Logical. If \code{TRUE}, the flow field is added to an existing
#' plot. If \code{FALSE}, a new plot is created. Defaults to \code{TRUE}.
#' @param xlab Label for the x-axis of the resulting plot.
#' @param ylab Label for the y-axis of the resulting plot.
#' @inheritParams .paramDummy
#' @param \dots Additional arguments to be passed to either
#' \code{\link[graphics]{plot}} or \code{\link[graphics]{arrows}}.
#' @return Returns a \code{\link[base]{list}} with the following components (the
#' exact make up is dependent on the value of \code{system}):
#' \item{add}{As per input.}
#' \item{arrow.head}{As per input.}
#' \item{arrow.type}{As per input.}
#' \item{col}{As per input, but with possible editing if a
#' \code{\link[base]{character}} \code{\link[base]{vector}} of the wrong
#' \code{\link[base]{length}} was supplied.}
#' \item{deriv}{As per input.}
#' \item{dx}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}. In the
#' case of a two-dimensional system, the values of the derivative of the first
#' dependent derivative at all evaluated points.}
#' \item{dy}{A \code{\link[base]{numeric}} \code{\link[base]{matrix}}. In the
#' case of a two-dimensional system, the values of the derivative of the second
#' dependent variable at all evaluated points. In the case of a one-dimensional
#' system, the values of the derivative of the dependent variable at all
#' evaluated points.}
#' \item{frac}{As per input.}
#' \item{parameters}{As per
#' input.}
#' \item{points}{As per input.}
#' \item{system}{As per input.}
#' \item{x}{A \code{\link[base]{numeric}} \code{\link[base]{vector}}. In the
#' case of a two-dimensional system, the values of the first dependent variable
#' at which the derivatives were computed. In the case of a one-dimensional
#' system, the values of the independent variable at which the derivatives were
#' computed.}
#' \item{xlab}{As per input.}
#' \item{xlim}{As per input.}
#' \item{y}{A \code{\link[base]{numeric}} \code{\link[base]{vector}}. In the
#' case of a two-dimensional system, the values of the second dependent variable
#' at which the derivatives were computed. In the case of a one-dimensional
#' system, the values of the dependent variable at which the derivatives were
#' computed.}
#' \item{ylab}{As per input.}
#' \item{ylim}{As per input.}
#' @author Michael J Grayling
#' @seealso \code{\link[graphics]{arrows}}, \code{\link[graphics]{plot}}
#' @export
#' @examples
#' #See archived phaseR package for examples
flowField <- function(deriv, xlim, ylim, parameters = NULL, system = "two.dim",
                      points = 21, col = "gray", arrow.type = "equal",
                      arrow.head = 0.05, frac = 1, add = TRUE,
                      xlab = if (system == "two.dim") state.names[1] else "t",
                      ylab =
                        if (system == "two.dim") state.names[2] else
                          state.names[1], ...) {
  if (any(!is.vector(xlim), length(xlim) != 2)) {
    stop("xlim is not a vector of length 2, as is required")
  }
  if (xlim[2] <= xlim[1]) {
    stop("xlim[2] is less than or equal to xlim[1]")
  }
  if (any(!is.vector(ylim), length(ylim) != 2)) {
    stop("ylim is not a vector of length 2, as is required")
  }
  if (ylim[2] <= ylim[1]) {
    stop("ylim[2] is less than or equal to ylim[1]")
  }
  if (points <= 0) {
    stop("points is less than or equal to zero")
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (!is.vector(col)) {
    stop("col is not a vector as required")
  }
  if (length(col) > 1) {
    col               <- col[1]
    message("Note: col has been reset as required")
  }
  if (!(arrow.type %in% c("proportional", "equal"))) {
    stop("arrow.type must be set to either \"proportional\" or \"equal\"")
  }
  if (arrow.head <= 0) {
    stop("arrow.head is less than or equal to zero")
  }
  if (frac <= 0) {
    stop("frac is less than or equal to zero")
  }
  if (!is.logical(add)) {
    stop("add must be logical")
  }
 state.names <-  ifelse(system == "two.dim", c("x", "y"), "y")

  x                   <- seq(from = xlim[1], to = xlim[2], length = points)
  y                   <- seq(from = ylim[1], to = ylim[2], length = points)
  dx                  <- dy <- matrix(0, ncol = points, nrow = points)
  xmax.length         <- x[2] - x[1]
  ymax.length         <- y[2] - y[1]
  if (!add) {
    graphics::plot(1, xlim = c(xlim[1] - xmax.length, xlim[2] + xmax.length),
	                 ylim = c(ylim[1] - ymax.length, ylim[2] + ymax.length),
		               type = "n", xlab = xlab, ylab = ylab, ...)
  }
  if (system == "one.dim") {
    for (i in 1:points) {
      dy[1, i]        <- deriv(0, stats::setNames(c(y[i]), state.names),
                               parameters)[[1]]
    }
    for (i in 2:points) {
      dy[i, ]         <- dy[1, ]
    }
    abs.dy            <- abs(dy)
    abs.dy.non        <- abs.dy[which(abs.dy != 0)]
    max.abs.dy        <- max(abs(dy))
    coefficient       <-
      frac*min(xmax.length, ymax.length)/
        (2*sqrt(2)*max(sqrt(2*abs.dy.non/(abs.dy.non + (1/abs.dy.non))),
					             sqrt(2*(1/abs.dy.non)/(abs.dy.non + (1/abs.dy.non)))))
    for (i in 1:points) {
      for (j in 1:points) {
        if (dy[i, j] != 0) {
          factor      <- sqrt(2/(abs.dy[i, j] + (1/abs.dy[i, j])))
          y.shift     <- coefficient*factor*sqrt(abs.dy[i, j])
          x.shift     <- coefficient*factor/sqrt(abs.dy[i, j])
          if (dy[i, j] < 0) {
            y.shift   <- -y.shift
          }
        } else {
          y.shift     <- 0
          x.shift     <- coefficient*sqrt(2)
        }
        if (arrow.type == "proportional") {
          if (dy[i, j] != 0) {
            prop      <- abs.dy[i, j]/max.abs.dy
            y.shift   <- y.shift*prop
            x.shift   <- x.shift*prop
          } else {
            x.shift   <- y.shift*mean(abs.dy)/max.abs.dy
          }
        }
        graphics::arrows(x[i] - x.shift, y[j] - y.shift, x[i] + x.shift,
                         y[j] + y.shift, length = arrow.head, col = col, ...)
      }
    }
    return(list(add        = add,
                arrow.head = arrow.head,
                arrow.type = arrow.type,
                col        = col,
                deriv      = deriv,
                dy         = dy,
                frac       = frac,
                parameters = parameters,
                points     = points,
                system     = system,
                x          = x,
                xlab       = xlab,
                xlim       = xlim,
                y          = y,
                ylab       = ylab,
                ylim       = ylim))
  } else {
    for (i in 1:length(x)) {
      for (j in 1:length(y)) {
        df            <- deriv(0, stats::setNames(c(x[i], y[j]), state.names),
                               parameters)
        dx[i, j]      <- df[[1]][1]
        dy[i, j]      <- df[[1]][2]
      }
    }
    abs.dx            <- abs(dx)
    abs.dy            <- abs(dy)
    abs.dx.non        <- abs.dx[which(abs.dx != 0 & abs.dy != 0)]
    abs.dy.non        <- abs.dy[which(abs.dx != 0 & abs.dy != 0)]
    max.length        <- max(sqrt(dx^2 + dy^2))
    coefficient       <-
      frac*min(xmax.length, ymax.length)/
        (2*sqrt(2)*max(sqrt(2*(abs.dy.non/abs.dx.non)/
                              ((abs.dy.non/abs.dx.non) +
                                 (abs.dx.non/abs.dy.non))),
                       sqrt(2*(abs.dx.non/abs.dy.non)/
                              ((abs.dy.non/abs.dx.non) +
                                 (abs.dx.non/abs.dy.non)))))
    for (i in 1:points) {
      for (j in 1:points) {
        if (any(dx[i, j] != 0, dy[i, j] != 0)) {
          if (all(dx[i, j] != 0, dy[i, j] != 0)) {
            factor    <- sqrt(2/((abs.dy[i, j]/abs.dx[i, j]) +
                                   (abs.dx[i, j]/abs.dy[i, j])))
            y.shift   <- coefficient*factor*sqrt(abs.dy[i, j]/abs.dx[i, j])
            x.shift   <- coefficient*factor/sqrt(abs.dy[i, j]/abs.dx[i, j])
            if (dy[i, j] < 0) {
              y.shift <- -abs(y.shift)
            }
            if (dx[i, j] < 0) {
              x.shift <- -abs(x.shift)
            }
          }
          if (all(dx[i, j] == 0, dy[i, j] != 0)) {
            y.shift   <- coefficient*sqrt(2)
            x.shift   <- 0
            if (dy[i, j] < 0) {
              y.shift <- -abs(y.shift)
            }
          }
          if (all(dx[i, j] != 0, dy[i, j] == 0)) {
            y.shift   <- 0
            x.shift   <- coefficient*sqrt(2)
            if (dx[i, j] < 0) {
              x.shift <- -abs(x.shift)
            }
          }
          if (arrow.type == "proportional") {
            prop      <- sqrt((abs.dx[i, j]^2 + abs.dy[i, j]^2))/max.length
            y.shift   <- y.shift*prop
            x.shift   <- x.shift*prop
          }
          graphics::arrows(x[i] - x.shift, y[j] - y.shift, x[i] + x.shift,
                           y[j] + y.shift, length = arrow.head, col = col, ...)
        }
      }
    }
  }
  return(list(add        = add,
              arrow.head = arrow.head,
              arrow.type = arrow.type,
              col        = col,
              deriv      = deriv,
              dx         = dx,
              dy         = dy,
              frac       = frac,
              parameters = parameters,
              points     = points,
              system     = system,
              x          = x,
              xlab       = xlab,
              xlim       = xlim,
              y          = y,
              ylab       = ylab,
              ylim       = ylim))
}