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
  
      Our App wiseR comes equipped with the functionality to generate a                customDashboard package through the app itself. Thus the .Rbuildignore file       mentioned here belongs to the to be generated package and is essential to        its working. In no way is the file malcious or unnecessary, but is an            essential part of the wiseR package and hence this note should be ingnored. 



* checking installed package size ... NOTE  
  installed size is  8.2Mb   
  sub-directories of 1Mb or more:   
  bn    2.1Mb  
  doc   6.0Mb
  
      Both the sub-directories bn and doc have sizes more than 1Mb and necessarily     so. The app containes a single function wiseR() which initself calls the underlying shiny-application. The shiny-application is an interactive and very extensive dashboard containing thousands of lines of codes, and correspondingly uses a large array of custom-made functions. Not only this, but it also has build files for another customDashboard package which is modified and generated through the main wiseR dashboard. These all total to a large chunck of files with minimal to no wastefull code/files hence the large size. With regards to the doc folder, it consists of images and table required to build the wiseR vignette which is an extensive walktrough of how to use the app. The vignette is necessary as due to large and extensive functionality of the app it is essential to provide a proper tutorial of how to use the app along with a use case. Since this walktrough has large number of images (to maintain tutorial flow and ease of understanding) hence the large size
