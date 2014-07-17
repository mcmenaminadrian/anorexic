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

codefreq<-read.csv(opt$c, header=FALSE)
rwfreq<-read.csv(opt$r, header=FALSE)
colnames(codefreq)<-c('frame', 'count', 'freq')
colnames(rwfreq)<-c('frame', 'count', 'freq')
codemax<-apply(codefreq, 2, max, na.rm=TRUE)
rwmax<-apply(rwfreq, 2, max, na.rm=TRUE)
codemin<-apply(codefreq, 2, min, na.rm=TRUE)
rwmin<-apply(rwfreq, 2, min, na.rm=TRUE)

#get a page
GetACodePage <- function(freq) {
  randCode <- runif(1, codemin[3], codemax[3])
  possiblePages <- subset(codefreq, freq >= randCode)
  pageToGet <- subset(possiblePages, freq == (apply(possiblePages, 2, min))[3]) 
  return(pageToGet)
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
cat(opt$t)
cat("\" xmlns=\"http://cartesianproduct.wordpress.com\">\n")
#spawn some lines
opt$s
if (opt$s > opt$t) {
  for (i in opt$t : opt$s) {
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
codePage<-(GetACodePage())[1]

