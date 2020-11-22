# econocharts
Microeconomics/macroeconomics graphs made with ggplot2

This package allows creating microeconomics or macroeconomics charts in R with simple functions. This package inspiration is [reconPlots](https://github.com/andrewheiss/reconPlots) by Andrew Heiss.

THE PACKAGE IS UNDER HEAVY DEVELOPMENT. WORK IN PROGRESS. You can suggest ideas submiting an Issue

## TODO
- [ ] Finish documentation
- [ ] Price control
- [ ] Allow drawing custom functions
- [ ] Add graph for budget constraints
- [ ] Fix `linecol` argument
- [ ] Tax graph
- [ ] Shade producer and consumer surplus
- [ ] Add Edgeworth box


## Index
- [Installation](#installation)
- [Supply curve](#supply)
- [Demand curve](#demand)
- [Supply and demand](#supply-and-demand)
- [Indifference curves](#indifference-curves)
- [Production–possibility frontier](#productionpossibility-frontier)
- [Laffer curve](#laffer-curve)

## Installation

### GitHub
```r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("R-CoderDotCom/econocharts")
```

### CRAN
The package will be on CRAN as soon as possible

## Supply

```r
supply() # Default plot
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189128-fa523500-275f-11eb-9dbf-44cab8f0b5e3.png">
</p>


```r
supply(ncurves = 1,          # Number of supply curves to be plotted
      type = "line",         # Type of the curve
      x = c(2, 4, 5),        # Y-axis values where to create intersections
      linecol = 2,           # Color of the curves
      geom = "label",        # Label type of the intersection points
      geomfill = "pink",     # If geom = "label", is the background color of the label
      main = "Supply curve") # Title of the plot
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189293-cd525200-2760-11eb-8fb4-07a274c05473.png">
</p>

```r
supply(ncurves = 3, # Three supply curves
       xlab = "X",  # X-axis label
       ylab = "Y",  # Y-axis label
       bg.col = "lightblue") # Background color
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189367-24582700-2761-11eb-90e1-0024d611180c.png">
</p>


## Demand

```r
demand(x = 3:6,  # Intersections
      generic = FALSE) # Axis values with the actual numbers
```
<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189443-74cf8480-2761-11eb-9e41-f37fd0d19413.png">
</p>


```r
demand(main = "Demand", # Title
       sub = "curve",   # Subtitle
       xlab = "X",      # X-axis label
       ylab = "Y",      # Y-axis label
       names = "D[1]",  # Custom name for the curve
       geomcol = 2)     # Color of the custom name of the curve
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189716-c9bfca80-2762-11eb-94b1-e89ddddb5df7.png">
</p>


## Supply and demand

```r
sdcurve() # Default supply and demand plot
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189746-fd025980-2762-11eb-831f-65c68e895882.png">
</p>


```r
# Custom data
supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
supply1

demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
demand1

supply2 <- data.frame(x = c(2, 10), y = c(1, 9))
supply2

demand2 <- data.frame(x = c(8, 2), y = c(2, 8))
demand2

p <- sdcurve(supply1,   # Custom data
             demand1,
             supply2, 
             demand2,
             equilibrium = TRUE, # Calculate the equilibrium
             bg.col = "#fff3cd") # Background color
p + annotate("segment", x = 2.5, xend = 3, y = 6.5, yend = 7,                # Add more layers
             arrow = arrow(length = unit(0.3, "lines")), colour = "grey50")
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189792-536f9800-2763-11eb-8848-0149b743e209.png">
</p>


## Indifference curves

```r
indifference() # Default indifference curve
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189813-769a4780-2763-11eb-95f3-a5f8cb7976a1.png">
</p>

```r
indifference(ncurves = 2,  # Two curves
             x = c(2, 4),  # Intersections
             main = "Indifference curves",
             xlab = "Good X",
             ylab = "Good Y",
             linecol = 2,  # Color of the curves
             pointcol = 2) # Color of the intersection points
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99190313-240e5a80-2766-11eb-863f-a205d4496f50.png">
</p>


```r
p <- indifference(ncurves = 2, x = c(2, 4), main = "Indifference curves", xlab = "Good X", ylab = "Good Y")

int <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(3, nrow(p$curve))), p$curve + 1))

p$p + geom_segment(data = int, aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
      geom_segment(data = int, aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
      geom_point(data = int, size = 3)
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189873-c547e180-2763-11eb-8b6a-7b43af760468.png">
</p>


```r
indifference(ncurves = 2,    # Two curves
             type = "pcom",  # Perfect complements
             main = "Indifference curves",
             sub = "Perfect complements",
             xlab = "Good X",
             ylab = "Good Y",
             bg.col = "#fff3cd", # Background color
             linecol = 1,  # Color of the curve
             pointcol = 2) # Color of the intersection points
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189904-e4df0a00-2763-11eb-90a5-41898cc33f9f.png">
</p>


```r
indifference(ncurves = 5,     # Five curves
             type = "psubs",  # Perfect substitutes
             main = "Indifference curves",
             sub = "Perfect substitutes",
             xlab = "Good X",
             ylab = "Good Y",
             bg.col = "#fff3cd", # Background color
             linecol = 1) # Color of the curve
```


<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189953-2b346900-2764-11eb-9539-a0e5c53bde5d.png">
</p>

## Production–possibility frontier

```r
ppf(x = 1:6, # Intersections
   main = "PPF",
   geom = "text",
   generic = TRUE, # Generic axis labels
   xlab = "X",
   ylab = "Y",
   labels = 1:6,
   acol = 3)$p
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99189976-4f904580-2764-11eb-8c02-8b6486c87e2d.png">
</p>

```r
p <- ppf(x = 4:6, # Intersections
        main = "PPF",
        geom = "text",
        generic = TRUE, # Generic labels
        labels = c("A", "B", "C"), # Custom labels
        xlab = "BIKES",
        ylab = "CARS",
        acol = 3)      # Color of the area

p$p + geom_point(data = data.frame(x = 5, y = 5), size = 3) +
  geom_point(data = data.frame(x = 2, y = 2), size = 3) +
  annotate("segment", x = 3.1, xend = 4.25, y = 5, yend = 5,
           arrow = arrow(length = unit(0.5, "lines")), colour = 3, lwd = 1) +
  annotate("segment", x = 4.25, xend = 4.25, y = 5, yend = 4,
           arrow = arrow(length = unit(0.5, "lines")), colour = 3, lwd = 1)
```


<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99190004-751d4f00-2764-11eb-85f1-d198e97bcdb0.png">
</p>


## Laffer curve
```r
laffer(ylab = "T", xlab = "t",
       acol = "lightblue", # Color of the area
       pointcol = 4)       # Color of the maximum point
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99190031-8fefc380-2764-11eb-83f5-629596437ef7.png">
</p>


```r
laffer(xmax = 20, # Modify the curve
       t = c(3, 6, 9), # Intersections
       generic = FALSE,
       ylab = "T",
       xlab = "t",
       acol = "lightblue", # Color of the area
       alpha = 0.6,        # Transparency of the area
       pointcol = 4)       # Color of the maximum point

```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/99190101-f379f100-2764-11eb-9c17-1673f2d93543.png">
</p>
