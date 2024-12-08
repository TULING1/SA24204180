## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
library(SA24204180)
set.seed(111)
x=runif(100,0,10)
z=rnorm(100,0,1)
y=2*x+1+z

results <- microbenchmark(
  rcpp_versiom = lrC(x, y),
  r_version = lm(y ~ x),
  times = 10
)
print(results)

## -----------------------------------------------------------------------------
set.seed(123)
data <- numeric(1000)

weights <- c(0.2, 0.8)
means <- c(0, 2)
sigma <- c(2, 4)

for (i in 1:1000){
 tmp <- sample(1:2, size = 1, prob = weights)
 if (tmp == 1){
 data[i] <- rnorm(1, mean = means[1], sd = sigma[1])
 } else {
 data[i] <- rnorm(1, mean = means[2], sd = sigma[2])
 }
}

h <- 0.2

x_grid <- seq(min(data), max(data), length.out = 1000)

density_estimate <- kde(data, h, x_grid)

hist(data, breaks = 50)

plot(x_grid, density_estimate, type = "l", main = "Kernel Density Estimation", xlab = "Data Value", ylab = "Density")

