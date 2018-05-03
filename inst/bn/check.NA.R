check.NA<-function(DiscreteData)
{
  if(sum(is.na(DiscreteData))>0)
  {
    shinyalert(c("Data has missing values, impute data under pre-process tabs"), type = "info")
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}
