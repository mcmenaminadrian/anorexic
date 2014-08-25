#!/usr/bin/env Rscript
#command line options
suppressWarnings(library("optparse"))
library("scatterplot3d")
option_list <- list(
  make_option(c("-f", "--file"), type="character", default = "optsumm_threads10.csv",
	help="Input file name")
)
opt <- parse_args(OptionParser(option_list=option_list))

rawopt <- read.csv(opt$file, header=TRUE)
rawlru2 <- read.csv("lru2summ_threads1-.csv", header=TRUE);
attach(rawdata)
scatterplot3d(Access, Idle/Pages, Ticks,  pch=20, xlim=range(c(0, 200000000)), ylim=(c(0, 20000000)), zlim=range(c(0, 20000000000)), color="red", angle=75, type="h")
par(new=TRUE)
attach(rawlru2)
scatterplot3d(Access, Idle/Pages, Ticks,  pch=20, xlim=range(c(0, 200000000)), ylim=(c(0, 20000000)), zlim=range(c(0, 20000000000)), color="blue", angle=75, type="h")

