#total number of cells
TotalCells <- nrow(Design)
for (i in 1:TotalCells){
  Row <- i
  MyResult <- MySimulationCell(Design = Design, RowOfDesign = Row, K = 2000 )
  #write output of one cell of the design
  save(MyResult, file =file.path("results",paste0("MyResult", "Row", Row,".Rdata" , sep ="")))
}
