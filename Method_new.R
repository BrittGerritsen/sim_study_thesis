#method new: robust Bayesian parameter estimation

Method_new<- function(SimData){
  res <- BESTmcmc(SimData$var1, SimData$var2, numSavedSteps = 10^4)
}
