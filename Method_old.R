#method old: Welch's t-test

Method_old<- function(SimData){
  formula <- Y~group
  res <- t.test(formula, data = SimData$data, var.equal = FALSE)
  return <- res}
