rm(list=ls(all=T))
gc()

install.packages(c('sqldf','plyr','xlsx','tm','ggplot2','RWeka','mixtools','topicmodels','maxent'))

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

#setwd("/Users/Rebecca/Dropbox/research/ANES/scripts")

#Merging ANES files
source("/Users/Rebecca/Dropbox/research/ANES/scripts/merge.R")

#performing unsupervised classification
source("/Users/Rebecca/Dropbox/research/ANES/scripts/unsupervised.R")

#performing unsupervised classification
#source("supervised.R")
