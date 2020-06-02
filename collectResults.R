#collect all results and put it in a matrix
#original design is in the matrix Design
#total number of rows of Design = TotalCells
#first load one workspace, for example, cell 1 of the Design matrix
#save the number of replications within that cell
Row <- 1
load(file.path("results",paste("MyResult", "Row", Row,".Rdata" , sep ="")))
K <- nrow(MyResult)
#initialize a very large matrix with number of rows = K * TotalCells
Results_sim <- matrix(NA, ncol = ncol(MyResult), nrow = K*TotalCells)
#fill in this matrix with the results that you obtained for each cell of the design (= each of the workspaces).
#thus loop over the rows of Design
timeNeeded <- rep(NA,TotalCells)
for (i in 1:TotalCells){
  Row <- i
  load(file.path("results",paste0("MyResult", "Row", Row,".Rdata" , sep ="")))
  Results_sim[(K*(i-1)+1):(i*K), ] <- MyResult
  timeNeeded[i] <- attr(MyResult,'time')['elapsed']
}

#repeate the Design matrix K times
Results_des <- Design[rep(1:nrow(Design),each=K),]
rownames(Results_des) <- 1:nrow(Results_des)
#rbind the vector 1:K TotalCelss times
Results_K <- do.call(what = rbind, args = replicate(TotalCells, matrix(c(1:K), ncol = 1), simplify = F))
#create the final results matrix
ResultsSimAll <-as.data.frame(cbind(Results_des, K = Results_K, Results_sim))
#give the columns the right name:
names(ResultsSimAll) <- c(names(Design), "K", "Method1", "Method2")
head(ResultsSimAll)

#time results
timeResults <- cbind(Design,timeNeeded)
save(ResultsSimAll, file = "AllResultsSim.Rdata")
#optionally save it as a .sav file
library(haven)
workingDirectory <- paste(getwd(), "/", sep = "")
filenameToSave <- paste(workingDirectory, "AllResultsSim", ".sav", sep = "")
write_sav(ResultsSimAll, filenameToSave)
