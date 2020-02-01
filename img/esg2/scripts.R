#### Test script for library esg2 ####
######################################

#---------- Installation of the package ----

# devtools is needed to install from a github repo

# devtools::install_github("arnaudbu/esg2")
library(esg2)
library(data.table)
library(reshape2)

#---------- Creation of a zero coupon curve object ----

# Initialization

swap <- fread("swap.csv", dec = ",")
eiopa <- fread("eiopa.csv", dec = ",")
swaptions <- fread("swaptions.csv", dec = ",", header = TRUE)
swaptions <- melt(swaptions, id.vars = c("mat"))
colnames(swaptions) <- c("mat", "tenor", "vol")
swaptions$tenor <- as.numeric(as.character(swaptions$tenor))
swaptions <- swaptions[mat <= 10 & tenor <=10]
swaptions <- swaptions[!is.na(vol)]

curve <- curvezc(method = "continuous",
                 rates = eiopa$rate / 100
)

cs <- curvezc(method = "continuous",
              rates = approx(swap$mat, swap$taux_swap, xout = 1:30)$y / 100)

# Visualization

curve

print(curve)

plot(curve)

#---------- Creation of a Swaptions object ----

# Initialization

maturities = swaptions$mat
tenors = swaptions$tenor
vols = swaptions$vol / 100

swaptions <- swaptions("lognormal", cs, maturities, tenors, vols, 2)

# Visualization

swaptions

print(swaptions)

plot(swaptions)

#---------- Rate model ----

# Definition of the model

g2model <- g2(curve, horizon = 50, nsimul = 10000)

# Calibration of the model on swaptions prices (~ 1 min on i5)

input_param <- data.frame(Parameter = c("a", "b", "sigma", "eta", "rho"),
                          Initial.point = c(0.1, 0.25, 0.05, 0.2, 0.8),
                          Min = c(0.001, 0.0001, 0.0001, 0.001, -1),
                          Max = c(0.3, 0.5, 0.1, 0.3, 1))

g2model <- calibrate(g2model, swaptions, maxIter = 1000)

# Generate correlated distributions

correl <- cbind(c(1,g2model@rho, 0.25),c(g2model@rho,1, 0), c(0.25, 0, 1))
W <- genW(correl, g2model@nsimul, g2model@horizon)

# Projection of the model

g2model <- project(g2model, Wx = W[,,1], Wy = W[,,2])

# Visualization

g2model

print(g2model)

plot(g2model)

# Deflator test

test_deflator(g2model)

# Get deflator table

def <- deflator(g2model)

# Get zero coupon table at time 10

zc10 <- zctable(g2model, 10)

#---------- Add an action model ----

# Definition

action <- bs(g2model,
             s0 = 100,
             vol = 0.2,
             div = 0.02,
             rho = -0.5,
             W = W[,,3])

# Visualization

action

print(action)

plot(action)

# Get the trajectories

trajAction <- traj(action)

# Martingal test

test_martingal(action)
