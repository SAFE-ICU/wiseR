---
title: "cran-comments"
author: "Shubham Maheshwari, Tavpritesh Sethi"
date: "September 22, 2018"
output: html_document
---

## Test environments

* windows 10, R 3.5.1
* ubuntu 16.04, R 3.5.1

## R CMD check results

0 ERRORs | 0 WARNINGs | 2 NOTES

* checking for hidden files and directories ... NOTE  
  Found the following hidden files and directories:         
  inst/bn/customDashboard/.Rbuildignore
  
wiseR comes equipped with the functionality to generate a customDashboard package through the app itself. Thus it generates a .Rbuildignore file as a part of the process of creating the customDashboard from within the app. It is a standard .Rbuildignore file which is essential and is not malicious, hence this note should be ignored. 



* checking installed package size ... NOTE  
  installed size is  8.2Mb   
  sub-directories of 1Mb or more:   
  bn    2.1Mb  
  doc   6.0Mb
  
The Shiny app containes a single function wiseR() which calls the underlying shiny-application. The application is an end-to-end Bayesian Decision Network software which is extensive in analytical scope, visualizations and interactivity, hence does not contain any wasteful code. Therefore, the sub-directory bn has size more than 1Mb as it is the engine of the app. With regards to the doc folder, it consists of images and tables required to build the wiseR vignette which is an extensive walktrough of how to use the app. The vignette is necessary as due to large and extensive functionality of the app it is essential to provide a proper tutorial of how to use the app along with a use case. Hence the doc folder is large to make the app well-documented and easy to use.
