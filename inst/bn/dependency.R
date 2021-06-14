dependency<-function()
{
  if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
  if(require('graph')==F)
  {
    BiocManager::install('graph')
  }
  if(require('RBGL')==F)
  {
    BiocManager::install('RBGL')
  }
  if(require('Rgraphviz')==F)
  {
    BiocManager::install('Rgraphviz')
  }
  if(require('gRbase')==F)
  {
    install.packages('gRbase')
  }
  if(require('gRain')==F)
  {
    install.packages('gRain')
  }
  if(!"RCy3" %in% installed.packages()){
    BiocManager::install("RCy3")
  }
  library(RCy3)
  if(require('reticulate')==F){
    install.packages("reticulate")
    conda_create("wiser")
    #conda_install("wiser", "pytorch")
    #conda_install("wiser", "matplotlib")
    #conda_install("wiser", "networkx")
    #conda_install("wiser", "pandas")
    #conda_install("wiser", "scipy")
    #conda_install("wiser", "scikit-learn")
  }
}
