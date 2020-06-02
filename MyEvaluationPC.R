#evaluation criterion for the frequentist method: 
#to determine the power of the method: 
#is the p-value of the test significant at 0.05 level?
#if yes, then the test rejected the null-hypothesis that the difference 
#between the groups is 0: i.e., the alternative hypothesis is correctly accepted
#if no, then the null hypothesis is incorrectly not rejected.

#evaluation criterion for the bayesian method: 
#to determine the power of the method: 
#does the entire HDI not include zero?
#if yes, then the test rejected the null-hypothesis that the difference 
#between the groups is 0: i.e., the alternative hypothesis is correctly accepted
#if no, then the null hypothesis is incorrectly not rejected.

MyEvaluationPCFrequentist <- function(MyAnalysisResult){
  res <- ifelse(MyAnalysisResult$p.value < 0.05, 1, 0)
  # 1 means: the alternative hypothesis is correctly accepted
  # 0 means: the null hypothesis is incorrectly not rejected
  return(res)
}
MyEvaluationPCBayesian <- function(MyAnalysisResult){
  MyAnalysisResult <- summary(MyAnalysisResult)
  res <- ifelse(!(MyAnalysisResult['muDiff','HDIlo'] < 0 && MyAnalysisResult['muDiff','HDIup'] > 0), 1, 0)
  # 1 means: the alternative hypothesis is correctly accepted
  # 0 means: the null hypothesis is incorrectly not rejected
  return(res)
}
