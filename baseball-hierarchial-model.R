#Libraries-----
library(rjags)
library(runjags)
source("2e/DBDA2E-utilities.R")
library(bayesplot)
library(ggplot2)
library(bayestestR)
library(purrr)
library(magrittr)


#Data-----
data <- read.csv("2e/BattingAverage.csv")

#Analysis File-----
source("2e/Jags-Ybinom-XnomSsubjCcat-MbinomBetaOmegaKappa.R")

#Generate chain-----
mcmc_coda <- genMCMC(data = data,
                     zName = "Hits",
                     NName = "AtBats",
                     sName = "Player",
                     cName = "PriPos",
                     numSavedSteps = 11000,
                     thinSteps = 20)


#Display Model Diagnostics-----
diag_dir <- "diagnostic-plots/baseball-model-"

for(parName in c("omega[1]", "omegaO", "kappa[1]", "kappaO", "theta[1]")) {
  diagMCMC(codaObject = mcmc_coda,
           parName = parName,
           saveName = diag_dir)
}

#Display Posterior Plots-----
posterior_dir <- "posterior-plots/baseball-model-"
plotMCMC(mcmc_coda,
         data = data,
         zName = "Hits",
         NName = "AtBats",
         sName = "Player",
         cName = "PriPos",
         compVal = NULL,
         diffCList = list(c("Pitcher", "Catcher"),
                          c("Catcher", "1st Base")),
         diffSList = list(c("Kyle Blanks", "Bruce Chen"),
                          c("Mike Leake", "Wandy Rodriguez"),
                          c("Andrew McCutchen", "Brett Jackson"),
                          c("ShinSoo Choo", "Ichiro Suzuki")),
         compValDiff = 0.0,
         saveName = posterior_dir)

