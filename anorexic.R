#!/usr/bin/env Rscript
#command line options
suppressWarnings(library("optparse"))
option_list <- list(
  make_option(c("-t", "--thread"), type="integer", default = 1,
	help="Specify thread number"),
  make_option(c("-c", "--code"), type="character", default="codepages.csv",
	help="Name of code page frequency csv file"),
  make_option(c("-r", "--readwrite"), type="character", default="rwpages.csv",
	help="Name of read-write page frequency csv file"),
  make_option(c("-s", "--spawn"), type="integer", default = 1,
	help="Generate spawn commands up to thread [number]"),
  make_option(c("-z", "--sizecode"), type="character",
        default="codeallocs.csv",
	help="Name of file with code alloc size data"),
  make_option(c("-i", "--sizerw"), type="character",
        default="memallocs.csv",
	help="Name of file with rw memory alloc size data")
)
opt <- parse_args(OptionParser(option_list=option_list))

codefreq<-read.csv(opt$code, header=FALSE)
rwfreq<-read.csv(opt$readwrite, header=FALSE)
colnames(codefreq)<-c('frame', 'count', 'freq')
colnames(rwfreq)<-c('frame', 'count', 'freq')
codemax<-apply(codefreq, 2, max, na.rm=TRUE)
rwmax<-apply(rwfreq, 2, max, na.rm=TRUE)
codemin<-apply(codefreq, 2, min, na.rm=TRUE)
rwmin<-apply(rwfreq, 2, min, na.rm=TRUE)
codelengths<-read.csv(opt$sizecode, header=FALSE)
rwlengths<-read.csv(opt$sizerw, header=FALSE)
colnames(codelengths)<-c('length', 'count', 'freq')
colnames(rwlengths)<-c('length', 'count', 'freq')
codelengthMin<-apply(codelengths, 2, min, na.rm=TRUE)
codelengthMax<-apply(codelengths, 2, max, na.rm=TRUE)
rwlengthMin<-apply(rwlengths, 2, min, na.rm=TRUE)
rwlengthMax<-apply(rwlengths, 2, max, na.rm=TRUE)

#code writing functions
GetACodePage <- function() {
  randCode <- runif(1, codemin[3], codemax[3])
  possiblePages <- subset(codefreq, freq >= randCode)
  pageToGet <- subset(possiblePages, freq == (apply(possiblePages, 2, min))[3])
  return(pageToGet)
}

GetACodePageLength <- function(mins, maxs, lengths) {
  randLength<-runif(1, mins[3], maxs[3])
  possibleLengths<-subset(lengths, freq >= randLength)
  lengthToGet <- subset(possibleLengths, freq == (apply(possibleLengths, 2, min))[3])
  return(lengthToGet)
}

WriteOutCode<- function(page, offset, lengthI) {
  writePoint <- page * 2^12 + offset
  localCount<-0
  instructions <- 0
  while(localCount < lengthI$length) {
    cat("<instruction address=\"")
    writePoint
    cat("**")
    cat(as.hexmode(writePoint))
    cat("\" size=\"")
    maxInst <- lengthI$length - localCount
    if (maxInst < 16) {
      theLength <- sample(1:maxInst, 1)
      cat(theLength)
      localCount <- localCount + theLength
      writePoint <- writePoint + theLength
    } else {
      theLength <- sample(1:16, 1)
      cat(theLength)
      localCount <- localCount + theLength
      writePoint <- writePoint + theLength
    }
    cat("\" />\n")
    instructions <- instructions + 1
  }
  return(instructions)
}

#RW writing functions
GetARWPage <- function() {
  randCode <- runif(1, rwmin[3], rwmax[3])
  possiblePages <- subset(rwfreq, freq >= randCode)
  pageToGet <- subset(possiblePages, freq == (apply(possiblePages, 2, min))[3])
  return(pageToGet)
}

GetARWPageLength <- function(mins, maxs, lengths) {
  randLength<-runif(1, mins[3], maxs[3])
  possibleLengths<-subset(lengths, freq >= randLength)
  lengthToGet <- subset(possibleLengths, freq == (apply(possibleLengths, 2, min))[3])
  return(lengthToGet)
}

WriteOutRW<- function(page, offset, lengthI) {
#  writePoint <- bitwShiftL(page, 12)
  writePoint <- page * 2^12 + offset
  localCount<-0
  instructions <- 0
  while(localCount < lengthI$length) {
    if (runif(1, 0, 1) > 0.9) {
      cat("<modify")
    } else {
      cat("<load")
    }
    cat(" address=\"")
    hexWrite <- as.hexmode(writePoint)
    cat(hexWrite)
    cat("\" size=\"")
    maxInst <- lengthI$length - localCount
    if (maxInst < 16) {
      theLength <- sample(1:maxInst, 1)
      cat(theLength)
      localCount <- localCount + theLength
      writePoint <- writePoint + theLength
    } else {
      theLength <- sample(1:16, 1)
      cat(theLength)
      localCount <- localCount + theLength
      writePoint <- writePoint + theLength
    }
    cat("\" />\n")
    instructions <- instructions + 1
  }
  return(instructions)
}

#write out the XML header
cat("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")
cat("\n")
cat("<!DOCTYPE threadml [\n")
cat("<!ELEMENT threadml (instruction|modify|store|load|spawn)*>\n")
cat("<!ATTLIST threadml version CDATA #FIXED \"0.1\">\n")
cat("<!ATTLIST threadml thread CDATA #REQUIRED>\n")
cat("<!ATTLIST threadml xmlns CDATA #FIXED")
cat(" \"http://cartesianproduct.wordpress.com\">\n")
cat("<!ELEMENT instruction EMPTY>\n")
cat("<!ATTLIST instruction address CDATA #REQUIRED>\n")
cat("<!ATTLIST instruction size CDATA #REQUIRED>\n")
cat("<!ELEMENT modify EMPTY>\n")
cat("<!ATTLIST modify address CDATA #REQUIRED>\n")
cat("<!ATTLIST modify size CDATA #REQUIRED>\n")
cat("<!ELEMENT store EMPTY>\n")
cat("<!ATTLIST store address CDATA #REQUIRED>\n")
cat("<!ATTLIST store size CDATA #REQUIRED>\n")
cat("<!ELEMENT load EMPTY>\n")
cat("<!ATTLIST load address CDATA #REQUIRED>\n")
cat("<!ATTLIST load size CDATA #REQUIRED>\n")
cat("<!ELEMENT spawn EMPTY>\n")
cat("<!ATTLIST spawn thread CDATA #REQUIRED>\n")
cat("]>\n")
cat("<threadml thread=\"")
cat(opt$thread)
cat("\" xmlns=\"http://cartesianproduct.wordpress.com\">\n")
#spawn some lines
#opt$s
if (opt$spawn > opt$thread) {
  for (i in (opt$thread + 1) : opt$spawn) {
    cat("<spawn thread=\'")
    cat(i)
    cat("\' />\n")
  }
}

#get some distributions
codeMins = c(apply(codefreq, 2, min, na.rm=TRUE))
codeMaxs = c(apply(codefreq, 2, max, na.rm=TRUE))
rwMins = c(apply(rwfreq, 2, min, na.rm=TRUE))
rwMaxs = c(apply(rwfreq, 2, max, na.rm=TRUE))

#pick a code page
instructionCount<-0
codePage<-(GetACodePage())[1]
startPoint<-sample(0:4095, 1)
lengthToUse <-GetACodePageLength(codelengthMin, codelengthMax, codelengths)[1]
instructionCount <- instructionCount +
    WriteOutCode(codePage$frame, startPoint, lengthToUse)

#Now can alternate between code and rw memory
lastWasCode <- TRUE
while(instructionCount < 1000000) {
  startPoint<-sample(0:4095, 1)
  if (lastWasCode == TRUE) {
    if (runif(1, 0, 1) > 0.7) {
      #code
      codePage<-(GetACodePage())[1]
      lengthToUse<-GetACodePageLength(codelengthMin, codelengthMax, codelengths)[1]
      instructionCount <- instructionCount +
        WriteOutCode(codePage$frame, startPoint, lengthToUse)
    } else {
      rwPage<-(GetARWPage())[1]
      lengthToUse<-GetARWPageLength(rwlengthMin, rwlengthMax, rwlengths)[1]
      instructionCount <- instructionCount +
        WriteOutRW(rwPage$frame, startPoint, lengthToUse)
      lastWasCode<-FALSE
    } 
  } else {
    #code
    codePage<-(GetACodePage())[1]
    lengthToUse<-GetACodePageLength(codelengthMin, codelengthMax, codelengths)[1]
    instructionCount <- instructionCount +
      WriteOutCode(codePage$frame, startPoint, lengthToUse)
    lastWasCode<-TRUE
  }
}
cat("</threadml>\n")

