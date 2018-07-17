dependency<-function()
{
  if(require('gRbase')==F)
  {
    install.packages('gRbase')
  }
  if(require('gRain')==F)
  {
    install.packages('gRain')
  }
  if(!"RCy3" %in% installed.packages()){
    install.packages("BiocManager")
    BiocManager::install("RCy3")
  }
  library(RCy3)
}
