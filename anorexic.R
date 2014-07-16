#!/usr/bin/env Rscript
#command line options
suppressWarnings(library("optparse"))
option_list <- list(
  make_option(c("-t", "--thread"), type="integer", default=1,
	help="Specify thread number"),
  make_option(c("-c", "--code"), type="character", default="codepages.csv",
	help="Name of code page frequency csv file"),
  make_option(c("-r", "--readwrite"), type="character", default="rwpages.csv",
	help="Name of read-write page frequency csv file"),
  make_option(c("-s", "--spawn"), type="integer", default = 0,
	help="Generate spawn commands up to thread [number]")
)
opt <- parse_args(OptionParser(option_list=option_list))

codefreq<-read.csv(opt$c, header=FALSE)
rwfreq<-read.csv(opt$r, header=FALSE)
colnames(codefreq)<-c('frame', 'count', 'freq')
colnames(rwfreq)<-c('frame', 'count', 'freq')
codemax<-apply(codefreq, 2, max, na.rm=TRUE)
rwmax<-apply(rwfreq, 2, max, na.rm=TRUE)
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
if (opt$s > 0)

