################################################################################
# source('~/Desktop/project/syntax/00_start.R')
# source('~/Desktop/upf/courses/term3/dstopics/project/syntax/00_start.R')
################################################################################

# Paths
#PATH <- '/Users/miquel/Desktop/project/'
PATH <- '/Users/miquel/Desktop/upf/courses/term3/dstopics/project/'
SRCDIR <- paste(PATH, 'syntax/', sep = '')
DOCDIR <- paste(PATH, 'doc/', sep = '')
DATDIR <- paste(PATH, 'data/', sep = '')
INPDIR <- paste(PATH, 'input/', sep = '')
OUTDIR <- paste(PATH, 'output/', sep = '')
TMPDIR <- paste(PATH, 'temp/', sep = '')

# Dependencies
if (! require(EBImage)) {
  source('http://bioconductor.org/biocLite.R')
  biocLite()
  biocLite('EBImage')
}
library(jpeg)
library(randomForest)
library(mombf)
library(glmnet)
library(EBImage)
library(class)
library(e1071)
library(rpart)
library(xgboost)
#library(parallel)
#library(doMC)
#library(pracma)
#library(adabag)
#library(sgd)
#library(nnet)
#library(neuralnet)

# Functions
source(paste(SRCDIR, 'functions/galaxy_functions.R', sep = ''))
# END OF SCRIPT
