#' @title Indifference curves
#'
#' @description TODO
#'
#' @param ... Specify the curve or curves separated by commas (as `data.frame`) you want to display in the graph. This will override the sample curve.
#' @param ncurves If `...` is not specified, is the number of indifference curves to be generated based on the sample data.
#' @param xmax Numeric. Allows modifying the maximum X value for the default indifference function.
#' @param ymax Numeric. Allows modifying the maximum Y value for the default indifference function.
#' @param type Possible values are `"normal`, for a normal indifference function, `"psubs"` for perfect substitute and `"pcom"` for perfect complements.
#' @param x Y-axis values where to create intersections with the indifference curves.
#' @param pointcol If `x` is specified, is the color of the points that represents the intersections.
#' @param curve_names Boolean. If `TRUE`, the function adds default names to each.
#' @param names If `curve_names = TRUE` are custom names for the curves.
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
#' @param bg.col Background color of the plot.
#' @import ggplot2 dplyr
#' @export
indifference <- function(...,
                        ncurves = 1,
                        xmax,
                        ymax,
                        type = "normal",
                        x,
                        pointcol = 1,
                        curve_names = TRUE,
                        names,
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
                        bg.col = "white"){

  m <- FALSE

  match.arg(type, choices = c("normal", "psubs", "pcom"))

  if(missing(...)){
    ncurve <- ncurves

    if(missing(xmax)){
      xmax <- 9
    }

    if(missing(ymax)){
      ymax <- 9
    }

    if(type == "normal") {
      # Example indifference curve
      curve <- data.frame(Hmisc::bezier(c(0.9, xmax - 6, xmax),
                                        c(ymax, ymax - 6, 0.9)))

      m <- TRUE
    }

    if(type == "psubs") {
      curve <- data.frame(x = c(0.9, xmax),
                          y = c(ymax, 0.9))
      m <- TRUE
    }

    if(type == "pcom") {
      curve <- data.frame(x = c(rep(0.9, 10), seq(0.9, 9, length.out = 10)),
                          y = c(seq(0.9, 9, length.out = 10), rep(0.9, 10)))
      m <- TRUE
    }

  } else{

    curve <- list(...)

    class <- vector("character", length(curve))

    for(i in 1:length(curve)) {

      class[i] <- class(curve[[i]])

    }

    if(any(class != "data.frame")) {
      stop("You can only pass data frames to the '...' argument")
    }


    ncurve <- length(curve)
    if(ncurve == 1){
      m <- TRUE
    }
  }

  if(missing(names)) {
    names <- sapply(1:ncurves, function(i) paste0("I[", i, "]"))
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
    } else {

      # linecols <- vector("list", length = ncurves)
      #
      # for(i in 1:ncurves){
      #   linecols[[i]] <- rep(linecol[i], nrow(curve)/ ncurves)
      # }
      #
      # linecol <- unlist(linecols)

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


    if(type == "pcom") {
      warning("Intersections not available for perfect complements. Please add the points manually")
    } else {
    # Calculate the intersections of the curves
    intersections <- tibble()

    if(missing(...) | length(curve) == 1) {

      for(i in 1:length(x)) {
        intersections <- intersections %>%
          bind_rows(curve_intersect(data.frame(x = c(0, 10000), y = rep(x[i], 2)), data.frame(curve)))

      }
    } else {

      intersections <- vector("list", ncurve)
      for(i in 1:ncurve){

        for(j in 1:length(x)) {

          intersections[[i]][[j]] <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(x[j], nrow(curve[[1]]))), curve[[i]]))
        }

      }
      intersections <- bind_rows(intersections)
    }
    # print(intersections)
   }
  }

  p <- ggplot(mapping = aes(x = x, y = y))


  if(missing(...) | m){

    for(i in 0:(ncurves - 1)) {
      p <- p + geom_line(data = data.frame(curve) + i, color = linecol, size = 1, linetype = 1)
    }

  } else {

    for(i in 1:length(curve)) {
      p <- p + geom_line(data = data.frame(curve[[i]]), color = linecol[i], size = 1, linetype = 1)
    }
  }

  if(curve_names == TRUE) {

    if(ncurves == 1) {
      p <- p + annotate(geom = "text", x = max(as.data.frame(curve)$x) + 0.5, y = min(as.data.frame(curve)$y), label = "I",
                        size = 4, color = geomcol)
    } else {

      j <- 0
      for(i in 1:ncurves){
        p <- p + annotate(geom = "text", x = max(as.data.frame(curve)$x) + j + 0.5, y = min(as.data.frame(curve)$y) + j, label = names[i], parse = TRUE,
                          size = 4, color = geomcol)
        j <- j + 1
      }
    }

  }

  if(!missing(x) & type != "pcom"){

    p <- p + geom_segment(data = intersections,
                           aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +

      geom_segment(data = intersections,
                   aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
      geom_point(data = intersections, size = 3, color = pointcol)


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

        p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves),
                                    breaks = intersections$x, labels = sapply(length(x):1, function(i) as.expression(bquote(X[.(LETTERS[i])])))) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, max(unlist(curve)) + ncurves),
                             breaks = intersections$y, labels = sapply(length(x):1, function(i) as.expression(bquote(Y[.(LETTERS[i])]))))
      } else {

        labels <- sapply(length(intersections$x):1, function(i) as.expression(bquote(X[.(LETTERS[i])])))

        p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves),
                                    breaks = intersections$x, labels = labels) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,  max(unlist(curve)) + ncurves),
                             breaks = x, labels = sapply(length(x):1, function(i) as.expression(bquote(Y[.(LETTERS[i])]))))
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
