
# The number of points to create
n <- 200

# Set the range
xmin <- 10
xmax <- 100
ymin <- 10
ymax <- 100

# Sample from a Uniform distribution
x <- runif(200, 10, 100)
x
y <- runif(200, 10,100)
y

# See pre-defined variables
ls.str()


# Plot points and a rectangle

mapxy <- function(a = NA){
  plot(x, y, asp = a)
  rect(xmin, ymin, xmax, ymax)
}

mapxy()


# Load the spatstat package
library(spatstat)


# Create this many points, in a circle of this radius
n_points <- 300
radius <- 10

# Generate uniform random numbers up to radius-squared
r_squared <- runif(300, 2, 200)
angle <- runif(n_points, 0, 200*pi)

# Take the square root of the values to get a uniform spatial distribution
x <- sqrt(r_squared) * cos(angle)
y <- sqrt (r_squared)* sin(angle)


plot(radius) + points(x, y)


# Some variables have been pre-defined
ls.str()

# Set coordinates and window
ppxy <- ppp(x =x, y =y, window = disc(radius))

ppxy

# Test the point pattern
qt <- plot(ppxy)

# Inspect the results
plot(ppxy)
print(ppxy)

# Create a disc of radius 10
disc10 <- 10

# Compute the rate as count divided by area
lambda <- 500/ area(disc(10))

# Create a point pattern object
ppois <- rpoispp(lambda , window = disc10)

# Plot the Poisson point pattern
plot(ppois)

# Create a disc of radius 10
disc10 <-  disc(radius=10, centre=c(0,0))
disc10 
# Generate clustered points from a Thomas process
set.seed(123)
p_cluster <- rThomas(kappa = 0.35, scale = 1, mu = 3, win = disc10)
plot(p_cluster)

# Run a quadrat test
plot(p_cluster, alternative = "clustered")

quadrat.test(p_cluster, 
# Regular points from a Strauss process
set.seed(123)
p_regular <- rStrauss(beta = 2.9, gamma = 0.025, R = .5, W = disc(10))
plot(p_regular)

# Run a quadrat test
quadrat.test(p_regular, alternative="clustered")



