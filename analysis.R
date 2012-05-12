rm(list=ls(all=T))

library(sqldf)
library(plyr)
library(xlsx)
library(tm)
library(ggplot2)
library(RWeka)
library(mixtools)
library(topicmodels) #Allows for LDA
library(maxent) #Allows for maxent

#TODO: Switch over to RTextTools

#Merging ANES files
source("merge.R")

#performing unsupervised classification
source("unsupervised.R")