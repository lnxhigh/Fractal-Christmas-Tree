# Setting
wood <- "#3F301D"
staryellow <- "#F4C30080"
tree_green <- "darkgreen"

# Background - Delaunay triangulation
library(interp)

background <- function (N = 200, seedx = 1, seedy = 2, h = 0, s = 0, vmax = 1, vmin = 0.6) {
    set.seed(seedx)
    x <- runif(N)
    set.seed(seedy)
    y <- runif(N)
    
    tri <- tri.mesh(x, y)
    g <- triangles(tri)
    N <- nrow(g)
    gs <- matrix(NA, nrow = N, ncol=2)
    
    for (i in 1:N) {
        xgroup <- x[g[i, 1:3]]
        ygroup <- y[g[i, 1:3]]
        polygon(
            xgroup*10-5, ygroup*10-3.3, 
            col = hsv(h = h , s = s, v = 1 - (vmax-vmin)*mean(ygroup)), border = NA
        )
        gs[i, ] <- c(mean(xgroup)*10-5, mean(ygroup)*10-3.3)
    }
    
    return(invisible(gs))
}

# Chaos game Algorithm
move <- function (coord, choice, r = 0.5) {
    idx <- sample(seq_along(choice$x), size = 1)
    
    return (
        list(
            x = r*coord$x + (1-r)*choice$x[idx],
            y = r*coord$y + (1-r)*choice$y[idx],
            idx = idx
        )
    )
}

# Triangle (sierpinski variant)
triangle <- function (g = list(0, 0), r = 1, N = 50000, repetition = 2, zero = FALSE, plot = TRUE, ...) {
    
    theta <- 2*pi/3 * 0:2 + pi/2
    x <- g$x + r*cos(theta); y <- g$y + r*sin(theta)
    
    trixy <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("x", "y")))
    
    previous <- list(x = mean(choice$x), y = mean(choice$y), idx = 0)
    
    if (zero) {
        x <- c(rep(x, repetition), 0)
        y <- c(rep(y, repetition), 0)
    }
    
    choice <- list(x = x, y = y)
    
    for (i in 1:N) {
        current <- move(previous, choice, r = 0.5)
        trixy <- rbind(trixy, c(current$x, current$y))
        previous <- current
    }
    
    if (plot) {
        points(trixy, pch = '.', ...)
    }
    
    return (invisible(trixy))
}


library(data.table) # For rbind speedup
triangle_snow <- function (choice, N, col = "#00640040", p = 0.6, q = 0.1, ...) {
    
    previous <- list(x = mean(choice$x), y = mean(choice$y), idx = 0)
    
    for (i in 1:N) {
        
        current <- move(previous, choice, r = 0.5)
        
        if (previous$idx == 1) {
            col_random <- sample(c("white", col), size = 1, prob = c(p*0.7, 1-p*0.7)) 
        } else if (current$idx == 1) {
            col_random <- sample(c("white", col), size = 1, prob = c(p, 1-p)) 
        } else {
            col_random <- sample(c("white", col), size = 1, prob = c(q, 1-q)) 
        }
        
        treexycol <<- rbindlist(list(treexycol, list(x = current$x, y = current$y, col = col_random)))
        previous <- current
    }
}

# Tree
tree <- function (g = list(x = 0, y = 0), len = 1, depth = 15, direction = c(1, 2, 3), N = 10000, ...) {
    if (depth < 1) {
        return (NULL)
    }
    
    x1 <- g$x; x2 <- g$x - sqrt(3)/2 * len; x3 <- g$x + sqrt(3)/2 * len
    y1 <- g$y + len; y2 <- g$y - len/2; y3 <- g$y - len/2
    
    # polygon(c(x1, x2, x3), c(y1, y2, y3), border = NA, ...)
    triangle_snow(choice = list(x = c(x1, x2, x3), y = c(y1, y2, y3)), N = N, ...)
    
    if (direction == 1) {
        tree(g = list(x = g$x, y = 2/3*y1+1/3*g$y), len = len*0.85, depth = depth-1, direction = 3, N = ceiling(N*0.85*0.85), ...)
        tree(g = list(x = g$x, y = 2/3*y1+1/3*g$y), len = len*0.85, depth = depth-1, direction = 2, N = ceiling(N*0.75*0.75), ...)
        tree(g = list(x = g$x, y = 2/3*y1+1/3*g$y), len = len*0.85, depth = depth-1, direction = 1, N = ceiling(N*0.75*0.75), ...)
        
    } else if (direction == 2) {
        tree(g = list(x = x2*2/3 + g$x*1/3, y = y2*2/3 + g$y*1/3), len = len*0.75, depth = depth-1, direction = 2, N = round(N*0.75*0.75), ...)
    } else {
        tree(g = list(x = x3*2/3 + g$x*1/3, y = y3*2/3 + g$y*1/3), len = len*0.75, depth = depth-1, direction = 3, N = round(N*0.75*0.75), ...)
    }
}

# Sierpinski carpet
carpet <- function (g = list(0, 0), r = 1, N = 10000, repetition = 2, zero = FALSE, plot = TRUE, ...) {
    theta <- 2*pi/8 * 0:7
    d <- r*rep(c(1, sqrt(2)), 4)
    x <- g$x + d*cos(theta); y <- g$y + d*sin(theta)
    
    carpetxy <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("x", "y")))
    
    previous <- list(x = g$x, y = g$y)
    
    if (zero) {
        x <- c(rep(x, repetition), 0)
        y <- c(rep(y, repetition), 0)
    }
    
    choice <- list(x = x, y = y)
    
    for (i in 1:N) {
        current <- move(previous, choice, r = 1/3)
        carpetxy <- rbind(carpetxy, c(current$x, current$y))
        previous <- current
    }
    
    if (plot) {
        points(carpetxy, pch = '.', ...)
    }
    
    return (invisible(carpetxy))
}

# Star
star <- function (g = list(x = 0, y = 0), r = 1, N = 10000, w = 157/256, col = "#F4C300", ...) {
    theta <- 2*pi/5 * 0:4 + pi/2
    x <- g$x + r*cos(theta); y <- g$y + r*sin(theta)
    choice <- list(x = x, y = y)
    
    previous <- list(x = g$x, y = g$y, idx = 0)
    current <- move(previous, choice, r = w)
    m <- length(choice$x) - 1
    
    for (i in 1:N) {
        next_ <- move(current, choice, r = w)
        while (previous$idx == current$idx) {
            next_ <- move(current, choice, r = w)
            if (!((next_$idx - current$idx) %in% c(-m, -1, 1, m))) {
                break
            }
        }
        points(next_, pch = '.', col = col, ...)
        previous <- current
        current <- next_
    }
}

# Koch snowflake
snow <- function (g = list(x = 0, y = 0), r = 1, N = 10000, angle = 0, ...) {
    previous <- list(x = g$x, y = g$y)
    
    theta <- 2*pi/6 * 0:5 + angle
    x <- g$x + r*cos(theta); y <- g$y + r*sin(theta)
    x <- c(x, g$x); y <- c(y, g$y)
    choice <- list(x = x, y = y)
    
    for (i in 1:N) {
        current <- move(previous, choice, r = 1/3)
        points(current, pch = '.', ...)
        previous <- current
    }
}

# Shade
map <- function (x, y, shear = 2.081667, scaling = 0.75, base = -2.3) {
    y <- y - base
    return (data.frame(x = x + shear*y, y = y * scaling + base))
}