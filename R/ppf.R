#' @title Production–possibility frontier
#'
#' @description TODO
#'
#' @param ... Specify the production–possibility frontiers separated by comma (as `data.frame`) you want to display in the graph. This will override the sample curve.
#' @param xmax Numeric. Allows modifying the maximum X value for the default production–possibility frontier.
#' @param ymax Numeric. Allows modifying the maximum Y value for the default production–possibility frontier.
#' @param type Possible values are `"concave"` (default) and `"line"` to plot a concave or a linear production–possibility frontier function by default, respectively.
#' @param x Y-axis values where to create intersections with the production–possibility frontier
#' @param linecol Line color of the curves.
#' @param labels If `x` is specified are the labels for the intersection points.
#' @param generic Boolean. If `TRUE` and `x` is specified, the axis labels shows generic names. If `FALSE`, the axis labels are the actual data of the axis that corresponds to the intersection points.
#' @param geom Possible values are `"text"` to display the labels of the intersection points with text and `"label"` to show them with labels.
#' @param geomcol Color of the labels of the intersection points.
#' @param geomfill If `geom = "label"` is the background color of the label.
#' @param main Main title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab Name of the X-axis.
#' @param ylab Name of the Y-axis.
#' @param acol Color of the area of the below the production–possibility frontier
#' @param alpha Transparency of the colored area
#' @param bg.col Background color of the plot
#' @import ggplot2 dplyr
#' @export
ppf <- function(...,
                xmax,
                ymax,
                type = "concave",
                x,
                linecol,
                labels,
                generic = TRUE,
                geom = "text",
                geomcol = 1,
                geomfill = "white",
                main = NULL,
                sub = NULL,
                xlab = NULL,
                ylab = NULL,
                acol,
                alpha = 0.3,
                bg.col = "white"){

  m <- FALSE

  if(missing(...)){

    ncurve <- 1

    if(missing(xmax)){
      xmax <- 6.5
    }

    if(missing(ymax)){
      ymax <- 6.5
    }

    if(type == "concave") {
      # Example indifference curve
      curve <- data.frame(Hmisc::bezier(c(0, xmax - 1.5, xmax),
                                        c(ymax, ymax - 1.5, 0)))
      m <- TRUE
    }

    if(type == "line") {
      curve <- data.frame(x = c(0, xmax),
                          y = c(ymax, 0))
      m <- TRUE
    }
  } else{

    curve <- list(...)
    ncurve <- length(curve)

    class <- vector("character", ncurve)

    for(i in 1:ncurve) {

      class[i] <- class(curve[[i]])

    }

    if(any(class != "data.frame")) {
      stop("You can only pass data frames to the '...' argument")
    }


    if(ncurve == 1){
      m <- TRUE
    }
  }

  if(missing(linecol)){

    if(missing(...)){
      linecol <- 1
    }

    if(!missing(...) & ncurve == 1){
      linecol <- 1
    }

    if(!missing(...) & ncurve > 1){
      linecol <- rep(1, ncurve)
    }
  } else {

    if(!missing(...) & length(linecol) == 1){
      linecol <- rep(linecol, ncurve)
    }
  }

  if(missing(labels) & !missing(x)){
    labels <- LETTERS[1:length(x)]
  }

  if(!missing(x)){

    if(any(x < 0) | any(x > max(data.frame(curve)$y))) {
      warning("There are values on the 'x' argument lower than 0 or greater than the maximun value of the curve")
      x <- x[x <= max(data.frame(curve)$y)]
    }

    # Calculate the intersections of the curves
    intersections <- tibble()

    if(missing(...) | length(curve) == 1) {

    for(i in 1:length(x)) {
      intersections <- intersections %>%
        bind_rows(curve_intersect(data.frame(x = c(0, 10000), y = rep(x[i], 2)), data.frame(curve)))

    }
      }else {

        intersections <- vector("list", ncurve)
        for(i in 1:ncurve){

          for(j in 1:length(x)) {

            intersections[[i]][[j]] <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(x[j], nrow(curve[[1]]))), curve[[i]]))
          }
        }

        intersections <- bind_rows(intersections)
    }

    print(intersections)
  }

  p <- ggplot(mapping = aes(x = x, y = y))


  if(!missing(acol)){

    p <-  p + geom_ribbon(data = data.frame(curve),
                            aes(x = x,
                                ymax = y), ymin = 0,
                            alpha = alpha, fill = acol)
  }

  if(missing(...) | m){
    p <- p + geom_line(data = data.frame(curve), color = linecol, size = 1, linetype = 1)
  } else {

    for(i in 1:length(curve)) {
      p <- p + geom_line(data = data.frame(curve[[i]]), color = linecol[i], size = 1, linetype = 1)
    }
  }

  if(!missing(x)){
    p <- p +  geom_segment(data = intersections,
                 aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +

    geom_segment(data = intersections,
                 aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
    geom_point(data = intersections, size = 3)


    if(geom == "label") {
      for(i in 1:length(x)){

        p <- p + annotate(geom = "label", x = unlist(intersections[1][i, ]) + 0.25, y = unlist(intersections[2][i, ]) + 0.25, label = rev(labels)[i],
                          size = 4, fill = "white", color = geomcol)
      }
    }

    if(geom == "text") {
      for(i in 1:length(x)){

        p <- p + annotate(geom = "text", x = unlist(intersections[1][i, ]) + 0.25, y = unlist(intersections[2][i, ]) + 0.25, label = rev(labels)[i],
                          size = 4, color = geomcol)
      }
    }

  if(generic == FALSE) {
    p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve)) + 1), breaks = intersections$x, labels = round(intersections$x, 2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1), breaks = intersections$y, labels = round(intersections$y, 2))
  } else {

    if(ncurve == 1 | missing(...)){

    p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1),
                                breaks = intersections$x, labels = sapply(length(x):1, function(i) as.expression(bquote(X[.(LETTERS[i])])))) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve)) + 1),
                         breaks = intersections$y, labels = sapply(length(x):1, function(i) as.expression(bquote(Y[.(LETTERS[i])]))))
    } else {

      labels <- sapply(length(intersections$x):1, function(i) as.expression(bquote(X[.(LETTERS[i])])))

      p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1),
                                  breaks = intersections$x, labels = labels) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1),
                           breaks = x, labels =sapply(length(x):1, function(i) as.expression(bquote(Y[.(LETTERS[i])]))))
    }

   }
  } else {
    p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1))
  }

  p <- p + labs(x = xlab, y = ylab, title = main, subtitle = sub)

  p <- p +
    # coord_equal() +
    theme_classic() +
    theme(plot.title = element_text(size = rel(1.3)),
          # axis.text.x = element_text(colour = linecol),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 1),
          axis.title.x = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0), angle = 0, hjust = 1),
          plot.background = element_rect(fill = bg.col),
          plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"))

  if(!missing(x)){
    return(list(p = p, intersections = intersections, curve = curve))
  } else {
    return(list(p = p, curve = curve))
  }
}


# ppf(main = "ppf", xlab = "X", ylab = "Y")$p
#
# ppf(x = 1:7, main = "ppf",  geom = "label", generic = F, xlab = "X", ylab = "Y")$p + coord_equal()
# ppf(x = 1:6, main = "ppf",  geom = "label", generic = T, xlab = "X", ylab = "Y")$p
# ppf(x = 1:6, main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y")$p
# ppf(x = 1:6, main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y", labels = 1:6)$p
# ppf(x = 1:6, main = "PPF",  geom = "text", generic = T, xlab = "X", ylab = "Y", labels = 1:6, acol = 3)$p
# ppf(x = 1:6, main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y", acol = 4, linecol = 4)$p
#
# # Custom curves
# line1 <- data.frame(x = c(0, 6.5),
#                     y = c(6.5, 0))
#
# line2 <- data.frame(x = c(0, 5),
#                     y = c(6.5, 0))
#
# ppf(line1, x = 1:3, main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y")$p
# ppf(line1, line2, x = c(2, 5), main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y", linecol = 1:2)$p
#
#
#
# curve1 <- data.frame(Hmisc::bezier(c(0, 5, 6.5),
#                                   c(6.5, 5, 0)))
#
# curve2 <- data.frame(Hmisc::bezier(c(0, 2.5, 4),
#                                    c(6.5, 5, 0)))
#
# curve3 <- data.frame(Hmisc::bezier(c(0, 1.5, 3),
#                                    c(6.5, 5, 0)))
#
# ppf(curve1, x = 1:3, main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y")$p
# ppf(curve1, curve2, curve3, x = 2, main = "ppf",  geom = "text", generic = T, xlab = "X", ylab = "Y")$p
#
#
# # Ejemplo
# p <- ppf(x = 4:6, main = "PPF", geom = "text", generic = T, labels = c("A", "B", "C"), xlab = "BIKES", ylab = "CARS", acol  = 3)
#
# p$p + geom_point(data = data.frame(x = 5, y = 5), size = 3) +
#   geom_point(data = data.frame(x = 2, y = 2), size = 3) +
#   annotate("segment", x = 3.1, xend = 4.25, y = 5, yend = 5,
#            arrow = arrow(length = unit(1, "lines")), colour = 3, lwd = 1) +
#   annotate("segment", x = 4.25, xend = 4.25, y = 5, yend = 4,
#            arrow = arrow(length = unit(1, "lines")), colour = 3, lwd = 1)
#
#
#
# # Aprox more than 1 curve
# plot(line1, type = "l")
# lines(line2, col = 2)
#
# f <- approxfun(line1, rep(2, length(line1)), rule = 2)
#
#
# bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(2, nrow(line1))), line1),
#           curve_intersect(data.frame(x = 1:1000, y = rep(2, nrow(line1))), line2))
#
#
#
#
# x <- 1:3
# curves <- list(line1, line2)
#
# a <- vector("list", 2)
# for(i in 1:2){
#
#   for(j in 1:length(x)) {
#
#   a[[i]][[j]] <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(x[j], nrow(line1))), curves[[i]]))
#   }
# }
#
# a <- bind_rows(a)
#
#
#
# abline(h = 1:3, lty = 2)
# abline(v = a$x, lty = 2)
#
#
# abline(h = 2, lty = 2)
# abline(v = 3.46, lty = 2)


