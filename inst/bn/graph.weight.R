graph.weight<-function(boot,graphN){
  wList<-c()
  for(i in 1:dim(graphN)[1])
  {
    wList<-c(wList,boot[intersect(which(bn.hc.boot$to==graphN[i,2]),which(bn.hc.boot$from==graphN[i,1])),3])
  }
  return(wList)
}
