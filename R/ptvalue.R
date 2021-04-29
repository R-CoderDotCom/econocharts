
#' @title Value function in Prospect Theory
#'
#' @description Produces assymetric S-shaped value function according to lessons
#'   from Prospect Theory that losses are felt more intensely than gains.
#'
#' @param x Numeric. Vector of gain / loss values for x.
#' @param sigma Number. Exponent of functions, should be less than 1 to make an
#'   'S' shaped curve.
#' @param lambda Numer. Extent of assymetry between losses and gains. Should be
#'   less than -1 for losses to be more 'intense' than gains (as suggested by
#'   Prospect Theory). Between -1 and 0 for gains to be more intense than
#'   losses. Greater than 0 for losses to have positive value.
#' @param xint Numeric. Symmetric intersections. X-intercept values where to
#'   highlight points -- will be placed at both `xint` and `abs(xint)` to
#'   demonstrate assymetry in `value`.
#' @param xintcol Color of dashed lines calling-out `xint`.
#' @param main Main title of the plot.
#' @param sub Subtitle of the plot.
#' @param xlab Name of the X-axis.
#' @param ylab Name of the Y-axis.
#' @param col Color of function segment.
#' @param bg.col Background color.
#' @param ticks TOOD
#' @param xlabels TRUE / FALSE :  whether x labels are included.
#' @param ylabels TRUE / FALSE :  whether y labels are included.
#' @param by_x Number. Increment of the x-axis labels.
#' @param by_y Number. Increment of the x-axis labels.
#'
#' @details TODO
#'
#' @importFrom stats approxfun
#' @references Tversky, Amos; Kahneman, Daniel (1992). "Advances in prospect
#'   theory: Cumulative representation of uncertainty". Journal of Risk and
#'   Uncertainty. 5 (4): 297–323.
#' @examples
#'
#' ptvalue(
#'   sigma = 0.25,
#'   xint = 20,
#'   xintcol = 'blue',
#'   main = "Prospect Theory Shows That Gains & Losses are Felt Assymetrically",
#'   sub = "Losses are More Intense"
#' )
#'
#' @export
ptvalue <- function(x, sigma = 0.30, lambda = -2.25, xint, xintcol = 1,
                    main = NULL, sub = NULL, xlab = "Loss / Gain", ylab = "Value",
                    col = 1, bg.col = "white", ticks = TRUE,
                    xlabels =  TRUE, ylabels = TRUE, by_x = 10, by_y = 20){

  if(sigma >= 1) warning("sigma should be less than 1 to produce an 'S' shaped curve.")
  if(lambda >= -1) warning("lambda should be less than -1 in order that losses be represented as more intense than gains.")
  
  if(missing(x)) {

    x_pos <- seq(from = log(1), to = log(101), length.out = 1000) %>% 
      exp() %>% 
      {. - 1}
    
    x <- c(sort(-x_pos), 0, x_pos)
    
  }

  # Tversky & Kahneman, 1992
  value <- function(x, sigmaf = sigma, lambdaf = lambda) {

    if (x >= 0) {
      return(x ^ sigmaf)
    }

    if (x < 0) {
      return(lambdaf * (-x) ^ sigmaf)
    }
  }

  value <- Vectorize(value, vectorize.args = "x")

  maxv <- max(abs(value(x)))
  p <- ggplot(data = tibble(x = x), mapping = aes(x))


  if(ticks == TRUE) {
  # Axis ticks


  ## X-axis

  x_axis_max <- max(x)
  x_axis_min <- -x_axis_max

  tick_frame_x <- data.frame(ticks = seq(x_axis_min, x_axis_max, by = by_x), zero = 0) %>%
    subset(ticks != 0)


  ## Y-axis

  y_axis_max <- max(abs(value(x, sigma, lambda)))
  y_axis_min <- -y_axis_max

  tick_frame_y <- data.frame(ticks = seq(y_axis_min, y_axis_max, by = by_y), zero = 0) %>%
    subset(ticks != 0)



  tick_sz_y <- 0.02 * x_axis_max
  tick_sz_x <- 0.02 * y_axis_max

   p <- p + geom_segment(data = tick_frame_x,
                  aes(x = ticks, xend = ticks,
                      y = zero, yend = zero + tick_sz_x)) +
           geom_segment(data = tick_frame_y,
                        aes(x = zero, xend = zero + tick_sz_y,
                            y = ticks, yend = ticks))

  }


  # Labels

  if(xlabels == TRUE) {
    p <- p + geom_text(data = tick_frame_x, aes(x = ticks, y = zero, label = round(ticks,  2)), vjust = 1.5)
  }

  if(ylabels == TRUE) {
    p <- p + geom_text(data = tick_frame_y,  aes(x = zero, y = ticks, label = round(ticks, 2)), hjust = 1.25)
  }

  p <- p + geom_line(aes(x = x, y = value(x)), col = col)

  if(!missing(xint)) {

    curve <- data.frame(x = x, y = value(x))

    aprox <- approxfun(curve$x, curve$y)

    a <- aprox(xint)
    b <- aprox(-xint)
    len <- length(xint)

    data <- data.frame(xint = xint, y = rep(0, len), a = a, b = b)

      p <- p + geom_segment(data = data, aes(x = xint, y = y, xend = xint, yend = a), lty = "dashed", colour = xintcol) +
               geom_segment(data = data, aes(x = xint, y = a, xend = y, yend = a), lty = "dashed", colour = xintcol) +
               geom_point(data = data, aes(x = xint, y = a), size = 3) +

               geom_segment(data = data, aes(x = -xint, y = y, xend = -xint, yend = b), lty = "dashed", colour = xintcol) +
               geom_segment(data = data, aes(x = -xint, y = b, xend = y, yend = b), lty = "dashed", colour = xintcol) +
               geom_point(data = data, aes(x = -xint, y = b), size = 3)

  }

  p <- p + labs(x = ylab, y = xlab, title = main, subtitle = sub) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_y_continuous(limits = c(-maxv, maxv)) +
    theme_void() +
    theme(plot.title = element_text(size = rel(1.3)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0), angle = 0, hjust = 0.5),
          plot.background = element_rect(fill = bg.col),
          plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"))

  return(p)

}



ptvalue(col = 2, xint = seq(0, 100, 25), xintcol = 4,
        by_x = 25, by_y = 50,
        main = "Prospect Theory Value Function")

