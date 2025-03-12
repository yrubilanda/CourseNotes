#ALTERNATIVES TO ORDINARY LEAST SQUARES
#Maximum likelihood estimation
#a different approach, theoritically - trying to find parameter values that try to maximize the probability of data observed

library(tidyverse)
#generate a data set of 50 obs drawn from a normal dist that has a mean of 50 and std of 10
d <- tibble(val = rnorm(50, mean = 50, sd = 10))

ggplot(d) +
  geom_histogram(aes(x = val, y = after_stat(density))) +
  stat_function(fun = function(x) dnorm(x, mean = 50, sd = 10), color = "steelblue", linewidth = 1) +
  stat_function(fun = function(x) dnorm(x, mean = 65, sd =10), color = "violetred", linewidth = 1) +
  theme_bw()

val <-41
mean <- 50
sd <- 10

(likelihood <- 1/sqrt(2 * pi * sd^2) * exp((-(val - mean)^2)/(2 * sd^2)))

#or

(likelihood <- dnorm(val, mean, sd))

nll <- -1 * log(likelihood)#natural log

(summed_nll <- sum(nll))

#whats the liklihood of drawing these out
val <-c(41, 65, 10)
mean <- 65
sd <- 10

(likelihood <- 1/sqrt(2 * pi * sd^2) * exp((-(val - mean)^2)/(2 * sd^2)))

#or

(likelihood <- dnorm(val, mean, sd))

nll <- -1 * log(likelihood)#natural log

(summed_nll <- sum(nll)) #the likelihood of observing these values if they followed the mean and sd

#likelihood 
verbose_nll <- function(val, mu, sigma) { #function takes 3 variables
  likelihood <- 0
  ll <- 0
  for (i in 1: length(val)){
    likelihood[[i]] = dnorm(val[[i]], mean = mu, sd = sigma)
    ll[[i]] <- log(likelihood[[i]])
    message(paste0(val[[i]], " ", mean, " ", sd, " ", ll[[i]])) #returns the negative log likelihood of whatever set values you pass it of a normal distribution of a given mu and sigma
  }
  nll <- -1 * sum(ll)
  return(nll)
}

#50 is the blue and the 65 is the pink
#the lower negative log likliehood is more likley
val <- c(70, 75, 50)
mean <- 65 
sd <- 10

verbose_nll(val, mean, sd)

simple_nll <- function(mu, sigma, verbose = FALSE){
  ll = sum(dnorm(val, mean = mu, sd = sigma, log = TRUE))
  nll <- -1 * ll
  if(verbose == TRUE) {
    message(paste0("mean=", mu, "sigma=", sd, "nll=", nll))
  }
  return(nll)
}

simple_nll(mean, sd)

#so far calcualted likiehoods
#now want to find values for mu and sigma with highest likelihood
#using {optim} package so as to constrain sigma to be positive by setting the lower bound at 0
#install.packages("bbmle")
#install.packages("optim")
library(bbmle)
library(optim)

val <- rnorm(50, 50, 10) #50 samples out of a normal distribution with a mean of 50 and sd 10

(mle_norm <- bbmle::mle2(
  minuslogl = simple_nll,
  start = list(mu = 0, sigma = 1),
  method = "SANN", #simulated annealing method of optimization
  trace = TRUE))

mean(val)
sd(val)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

library(mosaic)

m <- lm(data = d, height~weight)
broom::tidy(m)
obs_slope <- broom::tidy(m) |>
  filter(term == "weight") |>
  pull(estimate)

nperm <- 1000
perm <- do(nperm) * {
  d_new <- d
  d_new$weight <- sample(d_new$weight)
  m <- lm(data = d_new, height~weight)
  broom::tidy(m) |>
    filter(term == "weight") |>
    pull(estimate)
}

perm # a vector perm of 1000 relationships between height and weight
histogram(perm$result) #centered at 0 because you have broken up the relationship
