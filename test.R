source("common.R")

# Background
par(pty = 's')
h <- 0.5916; s <- 0.67; vmax <- 1; vmin <- 0

m <- 0.5

plot.new()
plot.window(xlim = c(-4+m, 4-m), ylim = c(-2.3+m, 5.7-m))

background(N = 50, h = h, s = s, vmax = vmax, vmin = vmin)
background(N = 100, h = h, s = s, vmax = vmax, vmin = vmin)
background(N = 200, h = h, s = s, vmax = vmax, vmin = vmin)
background(N = 500, h = h, s = s, vmax = vmax, vmin = vmin)
background(N = 1000, h = h, s = s, vmax = vmax, vmin = vmin)

# Triangle (Sierpinski variant)
par(pty = 's', mar = rep(3, 4))
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
tri <- triangle(g = list(x = 0, y = 0), N = 50000)

plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
polygon(c(0, -sqrt(3)/2, sqrt(3)/2), c(1, -0.5, -0.5), lwd = 2)
points(tri[1:10,], type = 'b', pch = 16)

# Koch snowflake
par(bg = "black")
N <- 50000

par(pty = 's')
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
snow(N = N, angle = pi/6, col = "white")
par(bg = "white")

# Vicsek snowflake (Comparison with Koch snowflake)
par(bg = "black")

N <- 10000
theta <- 2*pi/4 * 0:3
x <- cos(theta); y <- sin(theta)
x <- c(x, 0); y <- c(y, 0)
choice <- list(x = x, y = y)

par(pty = 's')
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

for (i in 1:N) {
    current <- move(previous, choice, r = 1/3)
    points(current, pch = '.', col = "white")
    previous <- current
}

par(bg = "white")

# Star
par(bg = "black")
N <- 70000

par(pty = 's')
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
star(N = N)

plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
star(N = N, w = 0.5)

par(bg = "white")

# Sierpinski Carpet
par(pty = 's')
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
carpet(g = list(x = 0, y = 0), r = 1, N = 50000, zero = FALSE)
