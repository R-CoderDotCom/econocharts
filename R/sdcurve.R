#' @title Supply and demand chart
#'
#' @description econocharts
#'
#' @param ... Specify the demand and supply curve or curves separated by commas (as `data.frame`) you want to display in the graph, starting with supply. This will override the sample curves.
#' @param xmax Numeric. Allows modifying the maximum X value for the default functions.
#' @param ymax Numeric. Allows modifying the maximum Y value for the default functions.
#' @param generic  Boolean. If `TRUE`, the axis labels shows generic names. If `FALSE`, the axis labels are the actual data of the axis that corresponds to the intersection points between the two curves.
#' @param equilibrium Boolean. If `TRUE`, shows the intersection points between the two curves.
#' @param main Main title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab Name of the X-axis.
#' @param ylab Name of the Y-axis.
#' @param curve_names Boolean. If `TRUE`, the function adds default names to each.
#' @param names If `curve_names = TRUE`, are custom names for the curves.
#' @param linescol Color of the curves. It must be a vector of the same length as the number of displayed curves.
#' @param bg.col Background color of the plot.
#'
#' @import ggplot2 dplyr
#' @export
sdcurve <- function(...,
                    xmax,
                    ymax,
                    generic = TRUE,
                    equilibrium = TRUE,
                    main = NULL,
                    sub = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    curve_names = TRUE,
                    names,
                    linescol,
                    bg.col = "white") {

  # if(empirical == FALSE && missing(domain)){
  #   stop("Provide a domain for the empirical curves")
  # }

  if(missing(xmax)) {
    xmax <- 9
  }

  if(missing(ymax)) {
    ymax <- 9
  }

  if(missing(...)) {
    curves <-list(data.frame(Hmisc::bezier(c(1, 8, xmax),
                                     c(1, 5, xmax))), data.frame(Hmisc::bezier(c(1, 3, xmax),
                                                                            c(ymax, 3, 1))))
    ncurves <- 1

  } else {
    ncurves <- length(list(...))/2
    curves <- list(...)

    class <- vector("character", length(curves))

    for(i in 1:length(curves)) {

      class[i] <- class(curves[[i]])

    }

    if(any(class != "data.frame")) {
      stop("You can only pass data frames to the '...' argument")
    }

  }

  if(ncurves %% 2 == 0){
    par <- TRUE
  }

  if(missing(linescol)){
    linescol <- 1:length(curves)
  }

  # print(ncurves)
  # print(curves)

  if(equilibrium == TRUE) {

    # Calculate the intersections of the curves
    intersections <- tibble()
    j <- 2

    for(i in 1:ncurves) {
      intersections <- intersections %>%
        bind_rows(curve_intersect(data.frame(curves[j - 1]), data.frame(curves[j])))
      j <- j + 2
    }

    print(intersections)
  }

  # Max X Coordinates of the curves
  coord <- vector("list", length = length(curves))
  for(i in 1:length(curves)){

    coord[[i]] <- curves[[i]][which.max(curves[[i]][, 1]), ]
  }

  p <- ggplot(mapping = aes(x = x, y = y))

  for(i in 1:length(curves)) {
    p <- p + geom_line(data = data.frame(curves[i]), color = linescol[i], size = 1, linetype = 1)

   }

  if(equilibrium == TRUE) {
    p <- p + geom_segment(data = intersections,
                 aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
    geom_segment(data = intersections,
                  aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
    geom_point(data = intersections, size = 3)
  }

  # Curve labels

  if(curve_names == TRUE) {

    labelyfun <- numeric(length(curves))

    for(i in 1:length(curves)){

      labelyfun[i] <- approxfun(curves[[i]]$x, curves[[i]]$y)(max(coord[[i]] - 0.5))
    }

  if(!missing(names)) {

      for(i in 1:length(curves)){

        p <- p + annotate(geom = "label", x = max(coord[[i]] - 0.5), y = labelyfun[i], label = names[i], parse = TRUE,
                       size = 4, fill = i, color = "white")
      }

  } else {

    for(i in 1:length(curves)){

      l <- ifelse(i %% 2 == 0, "D", "S")

      p <- p + annotate(geom = "label", x = max(coord[[i]] - 0.5), y = labelyfun[i], label = l, parse = TRUE,
                        size = 4, fill = i, color = "white")
    }

   }

  }

  if(equilibrium == TRUE) {

    if(generic == TRUE){

      p <- p + scale_x_continuous(expand = c(0, 0), breaks = unique(intersections$x),limits = c(0,  max(unlist(curves)) + 1),
                                   labels = sapply(1:length(unique(intersections$x)), function(i) as.expression(bquote(Q[.(i)])))) +
        scale_y_continuous(expand = c(0, 0), breaks = unique(round(intersections$y, 2)), limits = c(0,  max(unlist(curves))  + 1),
                           labels = sapply(1:length(unique(round(intersections$y, 2))), function(i) as.expression(bquote(P[.(i)]))))

    } else {

      p <- p + scale_x_continuous(expand = c(0, 0), breaks = unique(intersections$x), limits = c(0,  max(unlist(curves)) + 1),
                                  labels = round(unique(intersections$x), 2)) +
        scale_y_continuous(expand = c(0, 0), breaks = unique(intersections$y), limits = c(0,  max(unlist(curves))  + 1),
                           labels = round(unique(intersections$y, 2)))
    }

  } else {

      p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curves)) + 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curves))  + 1))

    }

    p <- p + labs(x = xlab, y = ylab, title = main, subtitle = sub) +
      # coord_equal() +
      theme_classic() +
      theme(plot.title = element_text(size = rel(1.3)),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 1),
            axis.title.x = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0), angle = 0, hjust = 1),
            plot.background = element_rect(fill = bg.col),
            plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"))

    return(p)
}


# sdcurve(generic = F, xlab = "as")
#
# supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
# supply1
#
# demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
# demand1
#
# supply2 <- data.frame(x = c(2, 10), y = c(1, 9))
# supply2
#
# demand2 <- data.frame(x = c(8, 2), y = c(2, 8))
# demand2
#
#
# p <- sdcurve(supply1, demand1, supply2, demand1, equilibrium = T,  bg.col = "#fff3cd", generic = T)
# p +  annotate("segment", x = 2.5, xend = 3, y = 6.5, yend = 7,
#               arrow = arrow(length = unit(1, "lines")), colour = "grey50")
#
#
# sdcurve(supply1, demand1, supply2, demand1, labels = c("S[1]", "D[1]","S[2]", "D[1]"))


