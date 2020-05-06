# Review contents of LN supplied data

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03")

njd <- 0
nop <- 0
ncb <- 0
ndb <- 0

for(i in 0:11) {
  x <- read.table(gzfile(sprintf("%s%02.0f%s", "DukePart", i, ".csv.gz")), header=T, sep=",",
                  quote="\"", comment="", strip.white=T)
  if(max(nchar(x[,"judgestext"]))>njd)
    njd <- max(nchar(x[,"judgestext"]))
  if(max(nchar(x[,"opinionbytext"]))>nop)
    nop <- max(nchar(x[,"opinionbytext"]))
  if(max(nchar(x[,"concurbytext"]))>ncb)
    ncb <- max(nchar(x[,"concurbytext"]))
  if(max(nchar(x[,"dissentbytext"]))>ndb)
    ndb <- max(nchar(x[,"dissentbytext"]))
}

max(nchar(x[,"outcome"]))
