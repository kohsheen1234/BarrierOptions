
############## Part I
# Summary: Create a function called calc_mom() to calculate 
# the statistical moments of a vector of data.
# 
# The function calc_mom() should accept two arguments:
# - datav: A vector of data,
# - moment: An integer specifying the moment order.
# 
# calc_mom() should first remove NA values from datav using na.omit().
# 
# If moment=1 it should return the mean.
# If moment=2 it should return the variance.
# If moment=3 it should return the skewness.
# If moment=4 it should return the kurtosis.
# Otherwise it should produce an error.

calc_mom <- function(datav, moment) {
  
  datav <- na.omit(datav)
  nrows <- NROW(datav) 
  if (moment == 1) {
    # Mean
    return(mean(datav))
  }
  else if (moment == 2) {
    # Variance
    return(var(datav))
  }
  else if (moment == 3){
    datav <- (datav - mean(datav))/sd(datav)
    return(sum(datav^3)/(nrows))

  }
  else if (moment == 4){
    datav <- (datav - mean(datav))/sd(datav)
    return(sum(datav^4)/(nrows))
  }
  else {
    stop("The argument moment is not an integer > 0 and < 5")
  }
}

# Generate chi-squared random numbers (run this)
set.seed(1121)
datav <- rchisq(100, df=3)
datav[1] <- NA

# Run calc_mom() as follows, to verify it works correctly.
# You should get the following outputs:

calc_mom(datav, 1)
# [1] 3.024326
calc_mom(datav, 2)
# [1] 7.923505
calc_mom(datav, 3)
# [1] 1.79409
calc_mom(datav, 4)
# [1] 6.405079
#calc_mom(datav, 5)
# Error in calc_mom(datav, 5) : 
#   The argument moment is not an integer > 0 and < 5


# Calculate the mean, the variance, the skewness, and the 
# kurtosis for all the columns of rutils::etfenv$returns
# You can use the function sapply() twice, and the functions 
# calc_mom(), t(), colnames(), c(), and an anonymous 
# function.

library(rutils)

# create a matrix with the moments
momentv <- sapply(rutils::etfenv$returns, function(x) {
  sapply(c(1, 2, 3, 4), function(y) {
    calc_mom(x, y)
  })
})

# transpose the matrix and add column names
momentv <- t(momentv)
colnames(momentv) <- c("mean", "variance", "skewness", "kurtosis")
momentv

# momentv
#               mean     variance      skewness   kurtosis
# DBC  -1.893909e-05 1.575651e-04  -0.502143558   6.241208
# AIEQ  8.935013e-05 2.444058e-04  -0.966407843  11.168155
# IVE   2.204861e-04 1.524116e-04  -0.545172266  14.694900
# IEF   3.170650e-05 1.835676e-05   0.079906084   5.919314
# VTI   2.847330e-04 1.488165e-04  -0.474394385  15.181945
# EEM   1.830552e-04 3.185690e-04   0.046141077  18.358369
# XLB   2.481320e-04 2.246611e-04  -0.371648320  10.916247
# GLD   3.079138e-04 1.265519e-04  -0.292084121   9.333239
# USO  -5.224733e-04 5.677031e-04  -1.150071892  17.286781
# XLE   2.354731e-04 3.684536e-04  -0.710300668  15.976273
# XLF   4.345309e-05 3.612284e-04  -0.435175814  20.895910
# XLI   2.864655e-04 1.768311e-04  -0.406671927  12.369794
# VEU   1.260919e-06 2.125969e-04  -0.493435345  14.201706
# VXX  -2.863117e-03 2.807009e-03   0.801021626 268.240738
# IWB   2.821112e-04 1.451287e-04  -0.508382148  15.050844
# XLK   4.140153e-04 1.931291e-04  -0.220466537  12.890056
# VTV   2.091012e-04 1.511148e-04  -0.560871784  15.629449
# IWD   2.117091e-04 1.553893e-04  -0.467307291  16.293966
# SVXY  3.550143e-04 2.329753e-03 -17.800459946 631.436795
# VNQ   1.063723e-04 3.594213e-04  -0.520282358  20.549936
# IWF   3.429253e-04 1.533177e-04  -0.393859411  13.161033
# VYM   1.449387e-04 1.394605e-04  -0.493359679  14.449348
# IVW   3.176929e-04 1.493050e-04  -0.468071363  13.712631
# USMV  3.600948e-04 8.131133e-05  -0.856879974  23.588580
# XLP   2.583071e-04 8.037350e-05  -0.430243779  14.672494
# TLT   4.590438e-05 8.305805e-05   0.017280782   6.841441
# VLUE  1.620599e-04 1.551960e-04  -0.961549225  19.300737
# XLU   2.313004e-04 1.377780e-04  -0.003363005  17.920844
# XLV   3.023670e-04 1.131659e-04  -0.325273309  14.057185
# QUAL  3.536492e-04 1.289330e-04  -0.508319406  15.614252
# MTUM  3.815507e-04 1.536695e-04  -0.680284573  14.701411
# XLY   3.274129e-04 1.877316e-04  -0.566460142  11.603730

# Sort momentv on the mean column.
# You can use the function order().

momentv <- momentv[order(-momentv[, "mean"]), ]
momentv


# 
#               mean     variance      skewness   kurtosis
# XLK   4.140153e-04 1.931291e-04  -0.220466537  12.890056
# MTUM  3.815507e-04 1.536695e-04  -0.680284573  14.701411
# USMV  3.600948e-04 8.131133e-05  -0.856879974  23.588580
# SVXY  3.550143e-04 2.329753e-03 -17.800459946 631.436795
# QUAL  3.536492e-04 1.289330e-04  -0.508319406  15.614252
# IWF   3.429253e-04 1.533177e-04  -0.393859411  13.161033
# XLY   3.274129e-04 1.877316e-04  -0.566460142  11.603730
# IVW   3.176929e-04 1.493050e-04  -0.468071363  13.712631
# GLD   3.079138e-04 1.265519e-04  -0.292084121   9.333239
# XLV   3.023670e-04 1.131659e-04  -0.325273309  14.057185
# XLI   2.864655e-04 1.768311e-04  -0.406671927  12.369794
# VTI   2.847330e-04 1.488165e-04  -0.474394385  15.181945
# IWB   2.821112e-04 1.451287e-04  -0.508382148  15.050844
# XLP   2.583071e-04 8.037350e-05  -0.430243779  14.672494
# XLB   2.481320e-04 2.246611e-04  -0.371648320  10.916247
# XLE   2.354731e-04 3.684536e-04  -0.710300668  15.976273
# XLU   2.313004e-04 1.377780e-04  -0.003363005  17.920844
# IVE   2.204861e-04 1.524116e-04  -0.545172266  14.694900
# IWD   2.117091e-04 1.553893e-04  -0.467307291  16.293966
# VTV   2.091012e-04 1.511148e-04  -0.560871784  15.629449
# EEM   1.830552e-04 3.185690e-04   0.046141077  18.358369
# VLUE  1.620599e-04 1.551960e-04  -0.961549225  19.300737
# VYM   1.449387e-04 1.394605e-04  -0.493359679  14.449348
# VNQ   1.063723e-04 3.594213e-04  -0.520282358  20.549936
# AIEQ  8.935013e-05 2.444058e-04  -0.966407843  11.168155
# TLT   4.590438e-05 8.305805e-05   0.017280782   6.841441
# XLF   4.345309e-05 3.612284e-04  -0.435175814  20.895910
# IEF   3.170650e-05 1.835676e-05   0.079906084   5.919314
# VEU   1.260919e-06 2.125969e-04  -0.493435345  14.201706
# DBC  -1.893909e-05 1.575651e-04  -0.502143558   6.241208
# USO  -5.224733e-04 5.677031e-04  -1.150071892  17.286781
# VXX  -2.863117e-03 2.807009e-03   0.801021626 268.240738



############## Part II
# Summary: Perform Monte Carlo simulation of Brownian motion, 
# to estimate the price of a barrier option.
# Perform bootstrap simulation to estimate the standard error 
# of the estimated price.
# Apply antithetic sampling to the barrier option simulation 
# to reduce the standard error.

# (Actual asset prices follow Geometric Brownian motion, but
#  we will simplify and use Brownian motion, 
# and accept that prices can become negative.)

# About barrier options:
# One type of barrier option is a knock-out (down and out) 
# call option.
# The knock-out call option pays out the positive difference 
# between the final price minus the strike, but only if the 
# intermediate price never falls bellow the knock-out price.

# Define the simulation parameters:
# Number of steps in each simulation
nrows <- 1e3
# strike price
strikep <- 10
# knock-out barrier level
barl <- (-5)
# Number of Brownian motion simulations
nsimu <- 1e3



# Simulate multiple Brownian paths, by performing an sapply() 
# loop starting from 1 to nsimu.
# Inside the loop perform a vectorized simulation of Brownian 
# motion, and calculate the option pay-out, equal to the 
# positive difference between the final price minus the strike, 
# but only if the intermediate price never falls bellow the 
# knock-out price.
# The sapply() loop should return a numeric vector of length
# nsimu, called payouts.
# Hint: you can use an anonymous function that accepts an
# integer argument (the loop count) and returns a numeric
# value.
# Compare the simulated path values to the barl 
# level, to determine if at any point the path reached below 
# the barl.
# The comparison of the path with barl produces a Boolean
# vector, whose sum is zero only if the path never crossed 
# the barl, and is greater than zero if it did.


# Reset random number generator
set.seed(1121)
# Define Brownian motion parameters
sigmav <- 1.0 # Volatility
drift <- 0.0 # Drift

# Function to simulate a Brownian path and calculate the option payout
sim_path <- function(i) {
  datav <- rnorm(nrows, mean=drift, sd=sigmav)
  path <- cumsum(datav)
  if(min(path) > barl){
    payout <- max(path[nrows] - strikep, 0)
  } else {
    payout <- 0
  }
  return(payout)
}


# Simulate multiple Brownian paths and calculate option payout using sapply()
payouts <- sapply(1:nsimu, sim_path)

# Calculate the price of the barrier option as the 
# average value of payouts.
# You should get the following output:
mean(payouts)
# [1] 2.56747

# Perform a bootstrap of the simulation from p.1 above, 
# by repeating it in a parallel loop. 
# Inside the bootstrap loop calculate the price of the 
# barrier option as the average value of the payouts.
# The bootstrap loop should return a numeric vector of 
# length nboots, called bootd.


# Load package parallel
library(parallel)
# Get the number of cores to use in parallel
ncores <- detectCores() - 1
# Initialize compute cluster for parallel bootstrap under Windows
cluster <- makeCluster(ncores)

# Export the simulation parameters (under Windows):
# nrows, strikep, barl, and nsimu
# to the CPU cores using clusterExport().

sim_path_antithetic <- function(i) {
  datav <- rnorm(nrows, mean=drift, sd=sigmav)
  path1 <- cumsum(datav)
  path2 <- cumsum(-datav)
  payout1 <- ifelse(min(path1) > barl, max(path1[nrows] - strikep, 0), 0)
  payout2 <- ifelse(min(path2) > barl, max(path2[nrows] - strikep, 0), 0)
  payout <- mean(c(payout1, payout2))
  return(payout)
}


boot_func_arithmetic <- function(i) {
  payouts <- sapply(1:nsimu, sim_path_antithetic)
  price <- mean(payouts)
  return(price)
}


# Export the simulation parameters (under Mac-OSX or Linux):
clusterExport(cluster, c("nrows", "strikep", "barl", "nsimu","sigmav","drift","sim_path","sim_path_antithetic","boot_func_arithmetic"))

# Reset random number generator for all cores
# using clusterSetRNGStream():

clusterSetRNGStream(cluster, 1121)

# Number of bootstrap simulations
nboots <- 100

# Perform parallel bootstrap under Mac-OSX or Linux.

boot_func <- function(i) {
  payouts <- sapply(1:nsimu, sim_path)
  price <- mean(payouts)
  return(price)
}

bootd <- unlist(parSapply(cluster, 1:nboots, boot_func))


# Calculate the price of the barrier option as the 
# average value of bootd.
# Calculate the standard error as the standard deviation 
# of bootd.

# You should get results similar to the following:
mean(bootd)
# [1] 3.539408
sd(bootd)
# [1] 0.3735878

# Calculate the standard error of the mean of the 
# bootstrap values, that is calculate the standard 
# error of mean(bootd), and call it bootse

bootse <- sd(bootd) / sqrt(nboots)

# You should get a result similar to the following:
bootse
# [1] 0.03735878


# Apply antithetic sampling to the simulation from 
# p.1 above, and perform a bootstrap simulation for 
# it, as in p.2 above. 
# Hint: For each simulated path you can take (-path) as 
# another valid path, and calculate its option pay-out.

# Reset random number generator for all cores
clusterSetRNGStream(cluster, 1121)

# Perform parallel bootstrap under Mac-OSX or Linux.



bootd <- unlist(parSapply(cluster, 1:nboots, boot_func_arithmetic))

# Calculate the price of the barrier option as the 
# average value of bootd.
# Calculate the standard error as the standard deviation 
# of bootd.

# You should get results similar to the following:
mean(bootd)
# [1] 3.471071
sd(bootd)
# [1] 0.2346075

# Calculate the standard error of the mean of the 
# bootstrap values divided by bootse from p.2 above. 

# You should get a result similar to the following:
sd(bootd)/sqrt(nboots)/bootse

# [1] 0.6279849

# Which is close to:
sqrt(0.5)

# This demonstrates that antithetic sampling reduces 
# the standard error of the bootstrap values. 



# Plot a histogram of bootd.
# Add a curve with the Normal density, with the same mean and sd.
# Add a legend.
# You can use the functions hist(), lines(), density(), curve(), 
# format(), and legend().



# Create histogram
hist(bootd, breaks = 7, freq = FALSE, main = "Bootstrap of Barrier Option Simulation", xlab = "Option prices", ylab = "Density")

# Add normal density curve with the same mean and sd as the data
curve(dnorm(x, mean = mean(bootd), sd = sd(bootd)), add = TRUE, col = "red", lwd = 2)

# Add density curve
lines(density(bootd), col = "darkblue", lwd = 2)

# Add mean and standard deviation text labels
m <- mean(bootd)
s <- sd(bootd)
text(x = m - 0.2*s, y = max(density(bootd)$y), labels = paste0("Mean: ", round(m, 2)), pos = 2)
text(x = m - 0.2*s, y = max(density(bootd)$y) * 0.9, labels = paste0("SD: ", round(s, 2)), pos = 2)

# Add legend
legend("topright", legend = c("Density", "Normal Density"), fill = c("darkblue", "red"), col = "black", lwd = c(2, 2), bty = "n")

