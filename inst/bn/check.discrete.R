check.discrete<-function(DiscreteData)
{
  if(sum(sapply(DiscreteData,is.numeric))>0)
  {
    shinyalert("Data has numeric variables, use discretization options under pre-process tabs", type = "info")
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}
