#Libraries-----
library(rjags)
library(runjags)
source("2e/DBDA2E-utilities.R")
library(bayesplot)
library(ggplot2)
library(bayestestR)
library(purrr)
library(magrittr)

#Functions-----
compute_rope_pct <- function(ci, x){
  lower <- ci - 0.1*ci
  upper <- ci + 0.1*ci
  res <- mean(upper > x) - mean(lower > x)
  res
}
#Load Data-----
data <- read.csv("2e/z15N50.csv")
y <- data$y
Ntotal <- length(y)
dataList <- list(
  "y" = y,
  "Ntotal" = Ntotal
)

#Generate Model-----
modelString <- "
  model {
    for(i in 1:Ntotal) {
      y[i] ~ dbern(theta)
    }
    theta ~ dbeta(1, 1)
  }
"

#writeLines(modelString, con = "jags-models/coin-model.txt")

#Initialize Chains-----
initsList <- function() {
  resampledY <- sample(y, replace = TRUE)
  thetaInit = sum(resampledY) / length(resampledY)
  thetaInit = 0.001 + 0.998*thetaInit
  return (list(theta=thetaInit))
}

#Generate Chains-----
jagsModel <- jags.model(
  file = "jags-models/coin-model.txt",
  data = dataList,
  inits = initsList(),
  n.chains = 3, #default is 1
  n.adapt = 500 # default is 1000
)

#Burn-in Model-----
update(jagsModel, n.iter = 500)

#Generate Posterior Samples-----
codaSamples <- coda.samples(
  jagsModel,
  variable.names = c("theta"),
  n.iter = 3334
)

#Analyze Model-----

post_median <- map_dbl(codaSamples, median) %>% mean()
ci_hdi <- hdi(codaSamples, method = "HDI")
target <- 0.5
rope_low <- target - 0.1*target
rope_high <- target + 0.1*target
rope_interval <- c(rope_low, rope_high)
rope_pct <- map2_dbl(codaSamples, target, compute_rope_pct) %>% mean() * 100

hdi_title <- "Posterior distribution with 95% HDI"
median_title <- paste("Median = ", round(post_median, 3))
rope_title <- paste("ROPE Statistic = ", target, ", ",
                    "ROPE Interval = [", rope_interval[1], ", ", rope_interval[2],"], ",
                    "ROPE % = ", round(rope_pct, 2), "%")

plot_title <- ggtitle(paste0(hdi_title, "\n",
                             median_title, "\n",
                             rope_title))

mcmc_areas(codaSamples,
           pars = c("theta"),
           prob = 0.95) + 
  plot_title +
  vline_at(v=target, lty=2, col="sky blue") +
  vline_at(v = rope_interval, lty=2)








