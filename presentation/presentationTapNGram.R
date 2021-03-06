rm(list=ls())

library(devtools)

## Install Slidify
# install_github('slidify', 'ramnathv')
# install_github('slidifyLibraries', 'ramnathv')

## Load Slidify
library(slidify)
library(slidifyLibraries)

setwd("D:/WORK_2014/Certification_Data_Science/Data_Science_Capstone/TapNGram/presentation/")

author("presentation")

list.files()

slidify("index.Rmd")

browseURL("index.html")

getwd()

list.files()

#publish(user = "radmar2002", repo = "https://github.com/radmar2002/Presentation-Beer-Recommendation-Engine")
options(rpubs.upload.method = "internal")
publish(title = 'Beer_Rec_Engine', 'index.html', host = 'rpubs')
