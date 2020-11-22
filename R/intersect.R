#' @title Intersection of two curves
#'
#' @description Calculate where two lines or curves intersect. Curves are defined as data
#' frames with x and y columns providing cartesian coordinates for the lines.
#' This function works on both linear and nonlinear curves.
#'
#' @param curve1 Either a \code{data.frame} with columns named \code{x} and \code{y} or a function.
#' @param curve2 Either \code{data.frame} with columns named \code{x} and \code{y} or a function.
#' @param empirical If true (default) indicates that the curves are data frames of empirical data. If false, indicates that the curves are actual functions.
#' @param domain Two-value numeric vector indicating the bounds along the x-axis where the intersection should be found when \code{empirical} is false
#'
#' @details For now, \code{curve_intersect} will only find one intersection.
#'
#' If you define curves with empirical data frames (i.e. provide actual values
#' for x and y), ensure that \code{empirical = TRUE}.
#'
#' If you define curves with functions (i.e. \code{curve1 <- x^2}), ensure that
#' \code{empirical = FALSE} and provide a range of x-axis values to search for
#' an intersection using \code{domain}.
#'
#' @return A list with \code{x} and \code{y} values.
#'
#' @author
#' \itemize{
#' \item{Weiss, Andrew.}
#' }
#'
#' @importFrom stats approxfun uniroot
#'
#' @examples
#' # Straight lines (empirical)
#' line1 <- data.frame(x = c(1, 9), y = c(1, 9))
#' line2 <- data.frame(x = c(9, 1), y = c(1, 9))
#'
#' curve_intersect(line1, line2)
#'
#' # Curved lines (empirical)
#' curve1 <- data.frame(Hmisc::bezier(c(1, 8, 9), c(1, 5, 9)))
#' curve2 <- data.frame(Hmisc::bezier(c(1, 3, 9), c(9, 3, 1)))
#'
#' curve_intersect(curve1, curve2)
#'
#' # Curved lines (functional)
#' curve1 <- function(q) (q - 10)^2
#' curve2 <- function(q) q^2 + 2*q + 8
#'
#' curve_intersect(curve1, curve2, empirical = FALSE, domain = c(0, 5))
#' @export
curve_intersect <- function(curve1, curve2, empirical = TRUE, domain = NULL) {
  if (!empirical & missing(domain)) {
    stop("'domain' must be provided with non-empirical curves")
  }

  if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
    stop("'domain' must be a two-value numeric vector, like c(0, 10)")
  }

    if (empirical) {

      # Approximate the functional form of both curves
      curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
      curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)

      # Calculate the intersection of curve 1 and curve 2 along the x-axis
      point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x),
                         c(min(curve1$x), max(curve1$x)))$root

      # Find where point_x is in curve 2
      point_y <- curve2_f(point_x)
    } else {
      # Calculate the intersection of curve 1 and curve 2 along the x-axis
      # within the given domain
      point_x <- uniroot(function(x) curve1(x) - curve2(x), domain)$root

      # Find where point_x is in curve 2
      point_y <- curve2(point_x)
    }
  # }

  return(list(x = point_x, y = point_y))
}


# APROXIMAR CUANDO LA LINEA ES VERTICAL


# linerect <- data.frame(x = c(5,5), y = c(0, 9))
#
# line3 <- data.frame(x = c(2, 10), y = c(1, 9))
#
#
# curve_intersect(linerect, line3) # No va
#
# plot(linerect, type = "l")
# lines(line3, type = "l", col = 2)
#
# AF2 = approxfun(line3$x, line3$y)
# AF2(5)


