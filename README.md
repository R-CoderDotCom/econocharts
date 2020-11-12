# econocharts
Microeconomic graphs made with ggplot2

THE PACKAGE IS UNDER HEAVY DEVELOPMENT. WORK IN PROGRESS.

You can suggest ideas submiting an Issues

## Index
- [Installation](#installation)
- [Supply curve](#supply)
- [Demand curve](#demand)
- [Supply and demand](#supply-and-demand)

## Installation
Not possible yet

## Supply

```r
supply(ncurves = 2)
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98985645-1e293700-2524-11eb-8fb2-c6383536ffa3.png">
</p>


## Demand

```r
demand(ncurves = 2)
```
<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98985736-47e25e00-2524-11eb-8326-b915241e127d.png">
</p>

## Supply and demand

```r
supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
supply1

demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
demand1

supply2 <- data.frame(x = c(2, 10), y = c(1, 9))
supply2

demand2 <- data.frame(x = c(8, 2), y = c(2, 8))
demand2

p <- sdplot(supply1, demand1, supply2, demand2, equilibrium = TRUE,  bg.col = "#fff3cd")
p +  annotate("segment", x = 2.5, xend = 3, y = 6.5, yend = 7,
              arrow = arrow(length = unit(1, "lines")), colour = "grey50")
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98985920-824bfb00-2524-11eb-8a0c-f3ccdf0210f9.png">
</p>


## Indifference curves

```r
indifference()
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98983912-c2f64500-2521-11eb-8888-b5d740f6b55e.png">
</p>

```r
indifference(ncurves = 2,
             type = "psubs",
             x = c(2, 4),
             main = "Indifference curves",
             xlab = "Good X",
             ylab = "Good Y",
             bg.col = "#fff3cd",
             linecol = 1, 
             pointcol = 2)

```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98984125-0f418500-2522-11eb-9b44-d8639c601677.png">
</p>


```r
p <- indifference(ncurves = 2, x = c(2, 4), main = "indifference curves", xlab = "Good X", ylab = "Good Y")

int <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(3, nrow(p$curve))), p$curve + 1))

p$p + geom_segment(data = int, aes(x = 0, y = y, xend = x, yend = y), lty = "dotted")  +
  geom_segment(data = int,
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_point(data = int, size = 3)
```

ppf(x = 1:6, main = "PPF",  geom = "text", generic = T, xlab = "X", ylab = "Y", labels = 1:6, acol = 3)$p

## Production–possibility frontier


```r
ppf(x = 1:6, main = "PPF",  geom = "text", generic = T, xlab = "X", ylab = "Y", labels = 1:6, acol = 3)$p
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98984970-2f257880-2523-11eb-92f6-9d2e684b0a17.png">
</p>

```r
p <- ppf(x = 4:6, main = "PPF", geom = "text", generic = T, labels = c("A", "B", "C"), xlab = "BIKES", ylab = "CARS", acol  = 3)

p$p + geom_point(data = data.frame(x = 5, y = 5), size = 3) +
  geom_point(data = data.frame(x = 2, y = 2), size = 3) +
  annotate("segment", x = 3.1, xend = 4.25, y = 5, yend = 5,
           arrow = arrow(length = unit(1, "lines")), colour = 3, lwd = 1) +
  annotate("segment", x = 4.25, xend = 4.25, y = 5, yend = 4,
           arrow = arrow(length = unit(1, "lines")), colour = 3, lwd = 1)
```


<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98986238-ee2e6380-2524-11eb-9b52-c4e119491e5d.png">
</p>


## Laffer curve
```r
laffer(ylab = "T", xlab = "t", acol = "lightblue", pointcol = 4)
```

<p align="center">
 <img src="https://user-images.githubusercontent.com/67192157/98985326-aeb34780-2523-11eb-9708-61a6694e6d03.png">
</p>
