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
klru <- read.csv("2ksumm.csv", header=TRUE);

attach(rawopt)
scatterplot3d(Idle/Pages, Access, Ticks,  pch=20, ylim=range(c(0, 100000000)), xlim=(c(0, 1500000)), zlim=range(c(0, 20000000000)), color="red", angle=55 )
par(new=TRUE)
attach(rawlru2)
scatterplot3d(Idle/Pages, Access, Ticks,  pch=20, ylim=range(c(0, 100000000)), xlim=(c(0, 1500000)), zlim=range(c(0, 20000000000)), color="yellow", angle=55)
par(new=TRUE)
attach(klru)
scatterplot3d(Idle/Pages, Access, Ticks,  pch=20, ylim=range(c(0, 100000000)), xlim=(c(0, 1500000)), zlim=range(c(0, 20000000000)), color="blue", angle=55)

