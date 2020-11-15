#' @title demand curves
#'
#' @description TODO
#'
#' @param ... Specify the demand curve or curves separated by commas (as `data.frame`) you want to display in the graph. This will override the sample curve.
#' @param ncurves Number of demand curves to be generated based on the sample data.
#' @param xmax Numeric. Allows modifying the maximum X value for the default demand function.
#' @param ymax Numeric. Allows modifying the maximum Y value for the default demand function.
#' @param type Possible values are `"convex"` (default) and `"line"` to plot a convex or a linear demand function by default, respectively.
#' @param x Y-axis values where to create intersections with the demand curves.
#' @param curve_names Boolean. If `TRUE`, the function adds default names to each.
#' @param names If `curve_names = TRUE` are custom names for the curves.
#' @param linecol Line color of the curves.
#' @param labels If `x` is specified, are the labels for the intersection points.
#' @param generic Boolean. If `TRUE` and `x` is specified, the axis labels shows generic names. If `FALSE`, the axis labels are the actual data of the axis that corresponds to the intersection points.
#' @param geom Possible values are `"text"` to display the labels of the intersection points with text and `"label"` to show them with labels.
#' @param geomcol Color of the labels of the intersection points.
#' @param geomfill If `geom = "label"` is the background color of the label.
#' @param main Main title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab Name of the X-axis.
#' @param ylab Name of the Y-axis.
#' @param bg.col Background color of the plot.
#' @import ggplot2 dplyr
#' @export
demand <- function(...,
                   ncurves = 1,
                   # distance = 1,
                   xmax,
                   ymax,
                   type = "convex",
                   x,
                   curve_names = TRUE,
                   names, # Names of the demand curves
                   linecol,
                   labels, # Label points
                   generic = TRUE,
                   geom = "text",
                   geomcol = 1,
                   geomfill = "white",
                   main = NULL,
                   sub = NULL,
                   xlab = NULL,
                   ylab = NULL,
                   bg.col = "white") {

  if(!missing(labels)){

    if(length(labels) == 1) {
      if(labels == "") {
        labels <- rep("", length(x))
      }
    }

    if(length(labels) != length(x)) {
      warning(paste("The number of labels provided must be equal to the intersections, so length(labels) must be:", length(x) * ncurves))
    }

  }

  m <- FALSE

  if(missing(...)){
    ncurve <- ncurves

    if(missing(xmax)){
      xmax <- 9
    }

    if(missing(ymax)){
      ymax <- 9
    }

    if(type == "convex") {
      # Example indifference curve
      curve <- data.frame(Hmisc::bezier(c(1, 3, xmax),
                             c(ymax, 3, 1)))

      m <- TRUE
    }

    if(type == "line") {
      curve <- data.frame(x = c(xmax, 0.9),
                          y = c(ymax, 0.9))
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

  if(!missing(x)){

    if(any(x < 0) | any(x > max(data.frame(curve)$y))) {
      warning("There are values on the 'x' argument lower than 0 or greater than the maximun value of the curve")
      x <- x[x <= max(data.frame(curve)$y)]
    }

    # Calculate the intersections of the curves
    intersections <- tibble()

    if((missing(...) | length(curve) == 1) & ncurves == 1) {

      for(i in 1:length(x)) {
        intersections <- intersections %>%
          bind_rows(curve_intersect(data.frame(x = c(0, 10000), y = rep(x[i], 2)), data.frame(curve)))

      }
    } else {

      intersections <- vector("list", ncurve)

      if(ncurves > 1) {

        if(length(x) == 1) {
          w <- 0
          for(i in 1:ncurve){

            for(j in 1:length(x)) {

              intersections[[i]][[j]] <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(x[j], nrow(curve))), data.frame(x = curve$x + w, y = curve$y)))
              w <- w + 1
            }
          }

          intersections <- bind_rows(intersections)
        } else {
          stop("Multiple intersections with ncurves > 1 is not implemented yet")
        }

      } else {

        for(i in 1:ncurve){

          for(j in 1:length(x)) {

            intersections[[i]][[j]] <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(x[j], nrow(curve[[1]]))), curve[[i]]))
          }
        }

        intersections <- bind_rows(intersections)

      }
    }
    # print(intersections)
  }

  if(missing(labels) & !missing(x)){
    labels <- LETTERS[1:nrow(intersections)]
  }

  p <- ggplot(mapping = aes(x = x, y = y))


  if(missing(...) | m){

    for(i in 0:(ncurves - 1)) {
      p <- p + geom_line(data = data.frame(x = curve$x + i, y = curve$y), color = linecol, size = 1, linetype = 1)
    }

  } else {

    for(i in 1:length(curve)) {
      p <- p + geom_line(data = data.frame(curve[[i]]), color = linecol[i], size = 1, linetype = 1)
    }
  }

  if(curve_names == TRUE) {

    if(ncurves == 1) {

      if(missing(names)) {
        names <- "D"
      }

      p <- p + annotate(geom = "text", x = max(as.data.frame(curve)$x) + 0.5, y = min(as.data.frame(curve)$y), label = names, parse = TRUE,
                        size = 4, color = geomcol)
    } else {

      if(missing(names)) {
        names <- sapply(1:ncurves, function(i) paste0("D[", i, "]"))
      }

      j <- 0
      for(i in 1:ncurves){
        p <- p + annotate(geom = "text", x = max(as.data.frame(curve)$x) + j + 0.35, y = min(as.data.frame(curve)$xy), label = names[i], parse = TRUE,
                          size = 4, color = geomcol)
        j <- j + 1
      }
    }

  }

  if(!missing(x)) {
    p <- p + geom_segment(data = intersections,
                          aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +

      geom_segment(data = intersections,
                   aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
      geom_point(data = intersections, size = 3)


    if(geom == "label") {
      for(i in 1:nrow(intersections)){

        p <- p + annotate(geom = "label", x = unlist(intersections[1][i, ]) + 0.35, y = unlist(intersections[2][i, ]), label = labels[i],
                          size = 4, fill = geomfill, color = geomcol)
      }
    }

    if(geom == "text") {

      for(i in 1:nrow(intersections)){

        p <- p + annotate(geom = "text", x = unlist(intersections[1][i, ]) + 0.35, y = unlist(intersections[2][i, ]), label = labels[i],
                          size = 4, color = geomcol)
      }
    }

    if(generic == FALSE) {

      p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve)) + ncurves),
                                  breaks = intersections$x, labels = round(intersections$x, 2)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + 1),
                           breaks = unique(round(intersections$y, 2)), labels = unique(round(intersections$y, 2)))

    } else {

      if(ncurve == 1 & missing(...)){

        p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves),
                                    breaks = intersections$x, labels = sapply(length(x):1, function(i) as.expression(bquote(X[.(LETTERS[i])])))) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve)) + ncurves),
                             breaks = round(intersections$y, 2), labels = sapply(length(x):1, function(i) as.expression(bquote(Y[.(LETTERS[i])]))))
      } else {

        labels <- rev(sapply(length(intersections$x):1, function(i) as.expression(bquote(P[.(LETTERS[i])]))))

        p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves),
                                    breaks = intersections$x, labels = labels) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves),
                             breaks = x, labels = sapply(length(x):1, function(i) as.expression(bquote(Q[.(LETTERS[i])]))))
      }

    }
  } else {
    p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves))
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




#
# demand(ncurves = 2, x = 2)
