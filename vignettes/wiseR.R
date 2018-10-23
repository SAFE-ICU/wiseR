## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ---- include=FALSE------------------------------------------------------
table = read.csv('comparison.csv')

## ----echo=F,results='asis'-----------------------------------------------
knitr::kable(table,caption = 'A comparison of wiseR and other bayesian learning apps')

