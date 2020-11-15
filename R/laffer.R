#' @title Laffer curve
#'
#' @description TODO
#'
#' @param curve Specify a custom curve (as `data.frame`). This will override the sample curve.
#' @param t Y-axis values where to create intersections with the Laffer curve.
#' @param xmax Numeric. Allows modifying the maximum X value for the default Laffer curve.
#' @param ymax Numeric. Allows modifying the maximum Y value for the default Laffer curve.
#' @param pointcol Color of the point that represents the optimum point.
#' @param generic Boolean. If `TRUE` and `x` is specified, the axis labels shows generic names. If `FALSE`, the axis labels are the actual data of the axis that corresponds to the intersection points and the optimal point.
#' @param showmax If `TRUE`, shows the optimal point.
#' @param main Main title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab Name of the X-axis.
#' @param ylab Name of the Y-axis.
#' @param acol Color of the area of the curve.
#' @param alpha Transparency of the colored area.
#' @param bg.col Background color of the plot.
#' @import ggplot2 dplyr
#' @export
laffer <- function(curve, t, xmax, ymax, pointcol = 1, generic = TRUE, showmax = TRUE,
                   main = NULL, sub = NULL, xlab = NULL, ylab = NULL, acol, alpha = 0.3, bg.col = "white"){

  if(missing(ymax)) {
    ymax <- 5
  }

  if(missing(xmax)) {
    xmax <- 10
  }

  if(ymax > xmax) {
    stop("'ymax' must be lower or equal to 'xmax'")
  }

  if(missing(curve)){

      # Example laffer curve
      curve <- data.frame(Hmisc::bezier(c(0, ymax, xmax),
                                        c(0, xmax + 0.1, 0)))
  }

  if(!missing(t)){

    if(any(t < 0) | any(t > max(data.frame(curve)$y))) {
      warning("There are values on the 't' argument lower than 0 or greater than the maximun value of the curve")
      t <- t[t <= max(data.frame(curve)$y)]
    }

    # Calculate the intersections of the curves
    intersections <- tibble()

      for(i in 1:length(t)) {
        intersections <- intersections %>%
          bind_rows(curve_intersect(data.frame(curve[curve$x < max(curve$y),]), data.frame(x = c(0, 10000), y = rep(t[i], 2))))
      }

    for(i in 1:length(t)) {
      intersections <- intersections %>%
        bind_rows(curve_intersect(data.frame(curve[curve$x > max(curve$y),]), data.frame(x = c(0, 10000), y = rep(t[i], 2))))
    }
    # print(intersections)
  }

  p <- ggplot(mapping = aes(x = x, y = y))

  if(!missing(acol)){

    p <-  p + geom_ribbon(data = data.frame(curve),
                          aes(x = x,
                              ymax = y), ymin = 0,
                          alpha = alpha, fill = acol)
  }

  p <- p + geom_line(data = data.frame(curve), color = 1, size = 1, linetype = 1)

  if(showmax == TRUE) {
    p <- p +
      geom_segment(data = data.frame(curve[which.max(curve$y), ]),
                   aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
      geom_segment(data = data.frame(curve[which.max(curve$y), ]),
                   aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
      geom_point(data = curve[which.max(curve$y), ], size = 3, color = pointcol)
  }


  if(!missing(t)){

    p <- p + geom_segment(data = intersections,
                          aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
      geom_segment(data = intersections,
                   aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
      geom_point(data = intersections, size = 3)


    if(generic == FALSE){
      p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve$x)) + 1),
                                  breaks = intersections$x, labels = round(intersections$x, 2)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$y)) + 1),
                           breaks = c(intersections$y, max(curve$y)), labels = round(c(intersections$y, max(curve$y)), 2))
    } else {

      labels <- rev(sapply(length(intersections$x):1, function(i) as.expression(bquote(t[.(i)]))))

      p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$x)) + 1),
                                  breaks = c(intersections$x, curve[which.max(curve$y), ]$x), labels = c(labels, "t*")) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve$y)) + 1),
                           breaks = c(unique(intersections$y), curve[which.max(curve$y), ]$y), labels = c(rev(sapply(length(unique(intersections$y)):1, function(i) as.expression(bquote("T"[.(i)])))),  "T*") )
    }

  } else {

    if(generic == FALSE) {
      p <- p +  scale_x_continuous(expand = c(0, 0), limits = c(0, max(curve$x) + 1), breaks = round(curve[which.max(curve$y), ]$x, 2), labels = round(curve[which.max(curve$y), ]$x, 2)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(curve$y) + 1), breaks = round(max(curve$y), 2), labels = round(max(curve$y), 2))
    } else {
      p <- p +  scale_x_continuous(expand = c(0, 0), limits = c(0, max(curve$x) + 1), breaks = curve[which.max(curve$y), ]$x, labels = "t*") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(curve$y) + 1), breaks = max(curve$y), labels = "T*")

    }
  }

  p <- p + labs(x = xlab, y = ylab, title = main, subtitle = sub) +
    # coord_equal() +
    theme_classic() +
    theme(plot.title = element_text(size = rel(1.3)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 1),
          axis.title.x = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0), angle = 0, hjust = 1),
          plot.background = element_rect(fill = bg.col),
          plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"))

  if(missing(t)){
    return(list(p = p, curve = curve))
  } else {
    return(list(p = p, intersections = intersections, curve = curve))
  }

}

# Ejemplo
# laffer(ylab = "T", xlab = "t", acol = "lightblue", pointcol = 4, generic = T)





