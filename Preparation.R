#preparation of the analysis
###initialize the factors of your design:
samp1 <- c(10, 15, 30)
samp2 <- c(10, 15, 30)
es <- c(0, 0.2, 0.5, 0.8)
variance <- c(1.0, 4.0)
##and create the simulation design matrix (full factorial)
#design is a data.frame with all possible combinations of the factor levels
#each row of the design matrix represents a cell of your simulation design
distrType <- c('normal', 'normalSkewed', 'normalPlatykurtic', 'normalGamma')
Design <- expand.grid(distrType = distrType, samp1 = samp1, samp2 = samp2, es = es, variance = variance)
###preparation of the analysis:
#install the relevant R packages:
library(PearsonDS)
library(BEST)
library(HDInterval)
###source the relevant R functions:
source("MyDataGeneration.R")
source("Method_new.R")
source("Method_old.R")
source("MyEvaluationPC.R")
