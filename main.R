### Merry Christmas

## Setting
source("common.R")
par(bg = "ghostwhite", pty = 's', family = "mono", las = 1, xaxs = 'i', yaxs = 'i')

## Frame
plot.new()
plot.window(xlim = c(-4, 4), ylim = c(-2.3, 5.7))

## Background - Delaunay triangulation
gs <- background(N = 200)

## Trunk - Sierpinski Carpet
c1 <- carpet(list(x = -0.3, y = -0.8), r = 0.3, N = 1000, col = wood, plot = FALSE)
c2 <- carpet(list(x = 0.3, y = -0.8), r = 0.3, N = 1000, col = wood, plot = FALSE)
c3 <- carpet(list(x = 0, y = -1.7), r = 0.6, N = 4000, col = wood, plot = FALSE)
# c4 <- carpet(list(x = 0, y = -1.7), r = 0.2, N = 444, col = wood) # optional


## Tree - Recursion + Sierpinski Triangle

# It takes some time because of `rbind()` issue
# I cache this as treexycol.dat

# treexycol <- list(x = NULL, y = NULL, col = NULL)
# tree(g = list(x = 0, y = 0), len = 1, depth = 15, direction = 3, col = tree_green)
# tree(g = list(x = 0, y = 0), len = 1, depth = 15, direction = 2, col = tree_green)
# tree(g = list(x = 0, y = 0), len = 1, depth = 15, direction = 1, col = tree_green)

treexycol <- read.table("treexycol.dat", header = TRUE)
m <- 10000
i <- sample(1:NROW(treexycol), m, replace = FALSE)

## Shadow
s <- map(c1[c1[ ,2] < -0.7, 1], c1[c1[ ,2] < -0.7, 2])
points(s, pch = '.', col = rgb(0.5, 0.5, 0.5))

s <- map(c2[c2[ ,2] < -0.7, 1], c2[c2[ ,2] < -0.7, 2])
points(s, pch = '.', col = rgb(0.5, 0.5, 0.5))

s <- map(c3[ ,1], c3[ ,2])
s <- s[s$x>0.6, ]
points(s, pch = '.', col = rgb(0.5, 0.5, 0.5, 0.75))

s <- map(treexycol$x, treexycol$y)
s <- s[s$x>0.6 & s$x<4, ]
points(s, pch = '.', col = rgb(0.5, 0.5, 0.5, 0.25))

## Body
points(c1, pch = '.', col = wood)
points(c2, pch = '.', col = wood)
points(c3, pch = '.', col = wood)

points(treexycol$x[i], treexycol$y[i]+0.1, col = "white", pch = '.', cex = rep(1:4, 4:1))
points(treexycol, col = treexycol$col, pch = '.')

## Star - Chaos game (n = 5, distance = 1/2)
i <- which.min(gs[ ,1]^2 + (gs[ ,2]-4)^2)
points(0, 4, col = hsv(h = 0 , s = 0, v = -0.4*mean(y[g[i, 1:3]]) + 1), pch = 16, cex = 2)
star(list(x = 0, y = 4), r = 0.25, N = 1000, w = 0.5, col = staryellow)

## Snow - Koch snowflake
m <- 100

set.seed(4)
snowxy <- cbind(x = runif(m, -4, 4), y = runif(m, -2.3, 5.7), r = rbeta(m, 2, 2)*0.1+0.05 , col = runif(m, 0.9, 1.0))
for (i in 1:NROW(snowxy)) {
    snow(
        g = list(x=snowxy[i, "x"], y = snowxy[i, "y"]), 
        r = snowxy[i, "r"], N = round(8000*snowxy[i, "r"]^2), 
        col = rgb(snowxy[i, "col"], snowxy[i, "col"], snowxy[i, "col"]),
        angle = runif(1, 0, pi/3)
    )
}

## Title
title("Merry Christmas", family = "mono", cex.main = 2)
# axis(side=1); axis(side=2)
# box()
