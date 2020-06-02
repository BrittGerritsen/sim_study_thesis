#comparison of power of Bayesian parameter estimation test and power of t-test
#in distribution type 'normal', variable 2 is from a normal distribution with mean = 0 es*sqrt(variance), variance = variance, skewness = 0, and kurtosis = 3
#in distribution type 'normalSkewed', variable 2 is from a skewed normal distribution with mean = 0 es*sqrt(variance), variance = variance, skewness = 0.25, and kurtosis = 3
#in distribution type 'normalPlatykurtic', variable 2 is from a platykurtic normal distribution with mean = 0 es*sqrt(variance), variance = variance, skewness = 0, and kurtosis = 2
#in distribution type 'normalGamma', variable 2 is from a gamma distribution with mean = es*sqrt((variance+3)/2 - 4, shape = 3, location = 1, and scale = 1
#es is "effect size", which is a factor in the simulation design (either 0.2, 0.5, or 0.8) 
#samp1 is the sample size of group 1 (either 10, 15, or 30)
#samp2 is the sample size of group 2 (either 10, 15, or 30)
#variance is the the variance of each group (either 1 or 4)
MyDataGeneration <- function(samp1, samp2, es, variance, distrType){
  if (distrType == 'normal'){
    moments1 <- c(mean = 0, variance = variance, skewness = 0, kurtosis = 3)
    moments2 <- c(mean = 0 + es*sqrt(variance), variance = variance, skewness = 0, kurtosis = 3)
    var1 <- rpearson(n = samp1, moments = moments1)
    var2 <- rpearson(n = samp2, moments = moments2)
  }
  if (distrType == 'normalSkewed'){
    moments1 <- c(mean = 0, variance = variance, skewness = 0, kurtosis = 3)
    moments2 <- c(mean = 0 + es*sqrt(variance), variance = variance, skewness = 0.25, kurtosis = 3)
    var1 <- rpearson(n = samp1, moments = moments1)
    var2 <- rpearson(n = samp2, moments = moments2)
  }
  if (distrType == 'normalPlatykurtic'){
    moments1 <- c(mean = 0, variance = variance, skewness = 0, kurtosis = 3)
    moments2 <- c(mean = 0 + es*sqrt(variance), variance = variance, skewness = 0, kurtosis = 2)
    var1 <- rpearson(n = samp1, moments = moments1)
    var2 <- rpearson(n = samp2, moments = moments2)
  }
  if (distrType == 'normalGamma'){
    moments1 <- c(mean = 0, variance = variance, skewness = 0, kurtosis = 3)
    var1 <- rpearson(n = samp1, moments = moments1)
    var2 <- rpearsonIII(n = samp2, shape = 3, location = 1, scale = 1) - 4 + es*sqrt((variance+3)/2)
  }
  Y <- c(var1, var2)
  group <- as.factor(c(rep(1, samp1), rep(2,samp2)))
  dat <- data.frame(Y,group)
  return((list(data=dat,var1=var1,var2=var2)))
}
