MySimulationCell<- function(Design = Design, RowOfDesign = 1, K = 2){
  #input arguments:
  #design = designmatrix
  #RowOfDesign: number that refers to the row of the design matrix = one cell
  #K: Total number of replications = number of data sets generated in one cell
  #create matrix or dataframe to store the results:
  MyResult <- matrix(NA, nrow = K, ncol=2)
  #create a loop over the replications k = 1 to K:
  tmp <- proc.time()
  for (k in 1:K){
    #generate data
    #set a random number seed to be able to replicate the result exactly
    set.seed((k + 1000)*RowOfDesign)
    SimDat <- do.call(MyDataGeneration, Design[RowOfDesign,] )
    #analyze data set with Method_new
    MyAnalysisResult1 <- Method_new(SimDat)
    #analyze data set with Method_old
    MyAnalysisResult2 <- Method_old(SimDat)
    #evaluate the analysis results of Method_new (Result1) and Mehtod_old (Result2)
    MyResult1 <- MyEvaluationPCBayesian(MyAnalysisResult1)
    MyResult2 <- MyEvaluationPCFrequentist(MyAnalysisResult2)
    #store the results in the right row k of your result matrix:
    MyResult[k, ] <- c(MyResult1, MyResult2)
  }
  #save the time to run the analyses of K data sets in one cell of the design.
  time <- proc.time() - tmp
  attr(MyResult,'time')<-time
  return(MyResult)
}
