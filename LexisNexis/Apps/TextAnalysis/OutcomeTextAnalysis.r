# Duke University Law Appeals Analysis Outcome Text Analysis

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

#library(LexisNexis)
library(ggplot2)
library(igraph)
library(ggraph)
library(ggridges)
library(XML)

# Specify current working dir
setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\ModelsAnalysis\\OutcomeTextAnalysis")


#############################################################################################
#### Function:  execute query
#############################################################################################

execQuery <- function(sql) {
  library(DBI)
  library(RMySQL)
  # Use ssh port 3306 forwarding
  # Mac:  ssh -L 3306:127.0.0.1:3306 user@lexnex-smben-01.oit.duke.edu
  # Windows:  Install PuTTY and configure a connection for Host Name lexnex-smben-01.oit.duke.edu
  #           Under ssh, Source port=3306, Destination=127.0.0.1:3306
  db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals", user="lexnexReader", password="lnread")
  d <- dbGetQuery(db, sql)
  dbDisconnect(db)
  return(d)
}


#############################################################################################
#### Case and outcome types
#############################################################################################

#x <- execQuery("select distinct CaseType, CaseType as Description from CaseType")
#caseType <- setNames(x[,1], x[,2])
caseType <- c("Criminal"="Criminal", "Civil"="Civil", "CriminalCivil"="CriminalCivil", "None"="None")
outcomeType <- c("Affirmed"="Affirmed", "Reversed"="Reversed", "Denied"="Denied", "Vacated"="Vacated", "Other"="Other")

#############################################################################################
#### Construct list of key words, either to be omitted from or identified in text
#############################################################################################

pronouns <- tolower(scan("Pronouns.csv", what="character"))
omitWords <- tolower(scan("OmitWords.csv", what="character"))
latinLegal <- tolower(xmlToDataFrame("LatinLegalTerms.xml")[,1])
matchWords <- tolower(scan("Words-1.csv", what="character"))

###############################################################################################
#### Function:
#### Query outcome text and split into vector of words
#### Omit punctuation, numerals, pronouns, and words of length less than three
#### Convert to lower case
###############################################################################################

oWords <- function(caseType, outcomeType, omitWords, lengthThresh, omitNonAlpha, caseID=0) {

      sql <- paste(" select ch.Outcome",
                   " from   CaseHeader ch join CaseTypeComposite cty on ch.LNI=cty.LNI",
                   "        join CaseOutcomeType oty on ch.LNI=oty.LNI",
                   " where  1=1",
                   ifelse(caseType!="", paste(" and cty.CaseType='", caseType, "'", sep=""), ""),
                   ifelse(outcomeType!="", paste(" and oty.OutcomeType='", outcomeType, "'", sep=""), ""), sep="")
      outcome <- execQuery(sql)[,"Outcome"]
      # Omit punctuation, special symbols, numerals, pronouns, and words of length < three
      w <- unlist(lapply(unlist(strsplit(tolower(outcome), " ")), function(a)  gsub("[^a-z]", "", a)))
      k <- which(nchar(w)>2 & !w %in% omitWords)
      return(w[k])

}

###############################################################################################
#### Plot distribution of Latin legal terms by case type
###############################################################################################

# Aggregate Latin term frequency by case type
ldat <- do.call(
          rbind,
          apply(as.matrix(caseType), 1,
            function(type) {
              # Retrieve text for case type
              txt <- tolower(
                       execQuery(
                         paste(" select ch.Outcome",
                               " from   CaseHeader ch join CaseTypeComposite ty on ch.LNI=ty.LNI",
                               " where  ty.CaseType='", type, "'", sep=""))[,1])
              # Identify terms
              # Note that grep() returns a vector of text array indices that match
              # If an array element conatins multiple instances of a searched text, it is
              # counted once only
              ltn <- apply(as.matrix(latinLegal), 1, function(lt) length(grep(lt, txt)))
              k <- which(ltn>0)
              if(length(k)>0) {
                return(data.frame("caseType"=type, "term"=latinLegal[k], "n"=ltn[k]))
              } else {
                return(NULL)
              }
            }))

# Limit to terms with a minimum specified frequency
nmin <- 250
k <- which(ldat[,"term"] %in% ldat[which(ldat[,"n"]>=nmin),"term"] & !ldat[,"term"] %in% c("r", "i.e.", "res"))

#png("Review\\Images\\LatinLegalTermByCaseType.png", res=300, height=2400, width=2400)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=n), stat="identity", fill="blue3") +
  facet_wrap(~caseType) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="\nLexis Nexis Appeals Outcome Text",
       subtitle=paste("Distribution of Latin Legal Terms by Case Type\nTerms with at least ",
                      nmin, " Instances within at least One Case Type\n", sep=""),
       x="term", y="frequency")
dev.off()

###############################################################################################
#### Plot distribution of Latin legal terms by outcome type
###############################################################################################

# Aggregate Latin term frequency by outcome type
ldat <- do.call(rbind, apply(as.matrix(outcomeType), 1,
                         function(type) {
                           # Retrieve text for type
                           txt <- tolower(
                                    execQuery(
                                      paste(" select ch.Outcome",
                                            " from   CaseHeader ch join CaseOutcomeType ty on ch.LNI=ty.LNI",
                                            " where  ty.OutcomeType='", type, "'", sep=""))[,1])
                           # Identify terms
                           # Note that grep() returns a vector of text array indices that match
                           # If an array element conatins multiple instances of a searched text, it is
                           # counted once only
                           ltn <- apply(as.matrix(latinLegal), 1, function(lt) length(grep(lt, txt)))
                           k <- which(ltn>0)
                           data.frame("type"=type, "term"=latinLegal[k], "n"=ltn[k])
                         }))

# Limit to terms with a minimum specified frequency
nmin <- 250
k <- which(ldat[,"term"] %in% ldat[which(ldat[,"n"]>=nmin),"term"] & !ldat[,"term"] %in% c("r", "i.e."))

#png("Review\\Images\\LatinLegalTermByOutcomeType.png", res=300, height=2400, width=3000)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=n), stat="identity", fill="blue3") +
  facet_wrap(~type) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="\nLexis Nexis Appeals Outcome Text",
       subtitle=paste("Distribution of Latin Legal Terms by Outcome Type\nTerms with at least ",
                      nmin, " Instances within at least One Outcome Type\n", sep=""),
       x="term", y="frequency")
dev.off()


###############################################################################################
#### Plot frequencies of top occurring words by case type
###############################################################################################

# Aggregate word frequency by type
ldat <- do.call(rbind, apply(as.matrix(caseType), 1,
                         function(ty) {
                           w <- oWords(caseType=ty, outcomeType="", omitWords=c(pronouns, omitWords),
                                       lengthThresh=2, omitNonAlpha=T)
                           wag <- aggregate(rep(1, length(w)), by=list(w), sum)
                           data.frame("caseType"=ty, "term"=wag[,1], "n"=wag[,2])
                         }))

# Convert to proportions within type
ntype <- aggregate(ldat[,"n"], by=list(ldat[,"caseType"]), sum)
colnames(ntype) <- c("caseType", "n")
rownames(ntype) <- ntype[,"caseType"]
ldat[,"p"] <- ldat[,"n"]/ntype[ldat[,"caseType"],"n"]

# write.table(ldat, "OutcomeWordFrequencyCaseType.csv", row.names=F, col.names=T, sep=", ", quote=F)
# ldat <- read.table("OutcomeWordFrequencyCaseType.csv", header=T, sep=",")

# Limit to top occurring terms
ntop <- 20
k <- which(ldat[,"term"] %in% unique(ldat[order(ldat[,"p"], decreasing=T),"term"])[1:ntop])

#png("Review\\Images\\WordDistributionByClass.png", res=300, height=2400, width=2400)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=p), stat="identity", fill="blue3") +
  facet_wrap(~caseType, labeller=as_labeller(setNames(names(caseType), caseType))) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="\nLexis Nexis Appeals Outcome Text",
       subtitle=paste("Distribution of Top ", ntop, " Words by Case Type\n", sep=""),
       x="\nword", y="proportion\n")
dev.off()


###############################################################################################
#### Plot frequencies of top occurring words by outcome type
###############################################################################################

# Aggregate word frequency by op type
ldat <- do.call(rbind, apply(as.matrix(outcomeType), 1,
                         function(ty) {
                           w <- oWords(caseType="", outcomeType=ty, omitWords=c(pronouns, omitWords),
                                       lengthThresh=2, omitNonAlpha=T)
                           wag <- aggregate(rep(1, length(w)), by=list(w), sum)
                           data.frame("outcomeType"=ty, "term"=wag[,1], "n"=wag[,2])
                         }))

# Convert to proportions within class
nOutcome <- aggregate(ldat[,"n"], by=list(ldat[,"outcomeType"]), sum)
colnames(nOutcome) <- c("outcomeType", "n")
rownames(nOutcome) <- nOutcome[,"outcomeType"]
ldat[,"p"] <- ldat[,"n"]/nOutcome[ldat[,"outcomeType"],"n"]

# write.table(ldat, "OutcomeWordFrequencyOutcomeType.csv", row.names=F, col.names=T, sep=", ", quote=F)
# ldat <- read.table("OutcomeWordFrequencyOutcomeType.csv", header=T, sep=",")

# Limit to top occurring terms
ntop <- 50
k <- which(ldat[,"term"] %in% unique(ldat[order(ldat[,"p"], decreasing=T),"term"])[1:ntop])

#png("Review\\Images\\WordDistributionByOutcomeType.png", res=300, height=3000, width=2400)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=p), stat="identity", fill="blue3") +
  facet_wrap(~outcomeType, ncol=1, labeller=as_labeller(setNames(names(outcomeType), outcomeType))) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="\nLexis Nexis Appeals Outcome Text",
       subtitle=paste("Distribution of Top ", ntop, " Words by Outcome Type\n", sep=""),
       x="\nword", y="proportion\n")
dev.off()


###############################################################################################
#### Plot word proportions of one case type against another 
###############################################################################################

qtype <- caseType[c(1, 2)] 

# Aggregate proportions for both types
w <- oWords(caseType=qtype[1], outcomeType="", omitWords=c(pronouns, omitWords), lengthThresh=2, omitNonAlpha=T)
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
w <- oWords(caseType=qtype[2], outcomeType="", omitWords=c(pronouns, omitWords), lengthThresh=2, omitNonAlpha=T)
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

# Join by word
w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
colnames(w) <- c("term", "p1", "p2")

# Convert NA to 0
w[which(is.na(w[,"p1"])),"p1"] <- 0
w[which(is.na(w[,"p2"])),"p2"] <- 0

# Filter by p
pmin <- 0.0075
k <- which(w[,"p1"]>=pmin | w[,"p2"]>=pmin)

# Set y offset for text
tyoffset <- 0.0015

#png(paste("Review\\Images\\WordDistribution-CaseType-", qtype[2], "-by-", qtype[1], ".png", sep=""), res=300, height=2400, width=2400)
ggplot() +
  geom_abline(intercept=min(min(w[,"p1"], w[,"p2"])), slope=1, color="gray75") +
  geom_text(data=w[k,], aes(label=term, x=p1, y=p2+tyoffset), size=2) +
  geom_point(data=w, aes(x=p1, y=p2), color="blue3", alpha=0.35) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="Lexis Nexis Appeals Outcome Text",
       subtitle=paste("Pairwise Word Proportions for Case Types ", qtype[2], " and ", qtype[1], "\n", sep=""),
       x=qtype[1], y=qtype[2])
dev.off()


###############################################################################################
#### Plot word proportions of one outcome type against another 
###############################################################################################

qtype <- outcomeType[c(1, 2)] 

# Aggregate proportions for both types
w <- oWords(caseType="", outcomeType=qtype[1], omitWords=c(pronouns, omitWords), lengthThresh=2, omitNonAlpha=T)
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
w <- oWords(caseType="", outcomeType=qtype[2], omitWords=c(pronouns, omitWords), lengthThresh=2, omitNonAlpha=T)
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

# Join by word
w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
colnames(w) <- c("term", "p1", "p2")

# Convert NA to 0
w[which(is.na(w[,"p1"])),"p1"] <- 0
w[which(is.na(w[,"p2"])),"p2"] <- 0

# Filter text labels by p
pmin <- 0.0025
k <- which(w[,"p1"]>=pmin | w[,"p2"]>=pmin)

# Set y offset for text
tyoffset <- 0.00025

# Set axis limits
axlim <- 0.01

#png(paste("Review\\Images\\WordDistribution-OutcomeType-", qtype[2], "-by-", qtype[1], "-b.png", sep=""), res=300, height=2400, width=2400)
ggplot() +
  geom_abline(intercept=min(min(w[,"p1"], w[,"p2"])), slope=1, color="gray75") +
  geom_text(data=w[k,], aes(label=term, x=p1, y=p2+tyoffset), size=2.5) +
  geom_point(data=w, aes(x=p1, y=p2), color="blue3", alpha=0.35) +
  scale_x_continuous(limits=c(0, axlim)) +
  scale_y_continuous(limits=c(0, axlim)) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="\nLexis Nexis Appeals Outcome Text",
       subtitle=paste("Pairwise Word Proportions for Opinion Types ", qtype[2], " and ", qtype[1], "\n", sep=""),
       x=qtype[1], y=qtype[2])
dev.off()


##################################################################################################################
#### Plot network of leading and trailing word pairs
#### Query opinion text and split into vector of words
#### Omit punctuation, numerals, pronouns, and words of length less than three
#### Convert to lower case
#### Return vector of words (default) or data.frame of three vectors:  words,
#### opinion record ID (concatenated case ID, opinion ID, opinion sub ID), and word
#### index within opinion record
#### Construct network graph using word pair counts and correlation of pairs
###############################################################################################

# Select case type, opinion type, and minimum edge frequency for inclusion 
ktCase <- 2
ktOutcome <- 3
nedgethresh <- 40

# Retrieve outcome text
oText <- execQuery(paste(" select ch.Outcome",
                         " from   CaseHeader ch join CaseTypeComposite cty on ch.LNI=cty.LNI",
                         "        join CaseOutcomeType oty on ch.LNI=oty.LNI",
                         " where  1=1",
                         ifelse(caseType!="", paste(" and cty.CaseType='", caseType[ktCase], "'", sep=""), ""),
                         ifelse(outcomeType!="", paste(" and oty.OutcomeType='", outcomeType[ktOutcome], "'", sep=""), ""), sep=""))[,1]

# Split each paragraph into words
# Omit punctuation, special symbols, numerals, pronouns, and words of length < three
# Compose pairs of consecutive words 
w <- do.call(rbind,
             sapply(oText,
               function(a) {
                 # Split words and omit punctuation, special characters, and unwanted words
                 w <- gsub("[^a-z]", "", unlist(strsplit(tolower(a), " ")))
                 w <- w[which(nchar(w)>2 & !w %in% omitWords)]
                 if(length(w)>1) {
                   return(t(sapply(1:(length(w)-1), function(i) w[i:(i+1)])))
                 } else if(length(w)==1) {
                   return(t(c(w[1], NA)))
                 } else {
                   return(t(c(NA, NA)))
                 }}))

# Aggregate word pairs
wpair <- aggregate(rep(1, nrow(w)), by=list(w[,1], w[,2]), sum)
colnames(wpair) <- c("w1", "w2", "n12")

# Join to aggregated counts of leading and trailing word appearance
wpair <- merge(wpair, setNames(aggregate(rep(1, nrow(w)), by=list(w[,1]), sum), c("w1", "n1")), by.x="w1", by.y="w1")
wpair <- merge(wpair, setNames(aggregate(rep(1, nrow(w)), by=list(w[,2]), sum), c("w2", "n2")), by.x="w2", by.y="w2")[c("w1", "w2", "n1", "n2", "n12")]

# Compose vector of high frequency word to trailing word links
#kg <- which(wpair[,"n1"]>750 | wpair[,"n2"]>750)
kg <- which(wpair[,"n12"]>nedgethresh)
n <- length(kg)
wv <- rep("", 2*n)
wv[seq(1, 2*n-1, 2)] <- wpair[kg,"w1"]
wv[seq(2, 2*n, 2)] <- wpair[kg,"w2"]

# Construct network
# Note the igraph:: qualifier for when igraph package not loaded
x <- igraph::make_graph(wv)

# Construct node circle radii for beginning and ending edge node elements
# Use ratio of word count to max word count within beginning and ending groups
kr <- match(vertex_attr(x, "name"), wpair[kg,"w1"])
r1 <- wpair[kr,"n1"]/max(wpair[kr,"n1"], na.rm=T)
kr <- match(vertex_attr(x, "name"), wpair[kg,"w2"])
r2 <- wpair[kr,"n2"]/max(wpair[kr,"n2"], na.rm=T)

# Aggregate frequencies by word (individual directions and sum of both directions)
nfreq <- rep(0, length(vertex_attr(x, "name")))
names(nfreq) <- vertex_attr(x, "name")
for(i in kg) {
  #nfreq[wpair[i,"w1"]] <- nfreq[wpair[i,"w1"]]+wpair[i,"n1"]
  #nfreq[wpair[i,"w2"]] <- nfreq[wpair[i,"w2"]]+wpair[i,"n2"]
  nfreq[wpair[i,"w1"]] <- nfreq[wpair[i,"w1"]]+wpair[i,"n12"]
  nfreq[wpair[i,"w2"]] <- nfreq[wpair[i,"w2"]]+wpair[i,"n12"]
}

# Compute correlation of words in each pair
n11 <- wpair[kg,"n12"]
n00 <- sum(wpair[,"n12"])-wpair[kg,"n1"]-wpair[kg,"n2"]+wpair[kg,"n12"]
n10 <- wpair[kg,"n1"]-wpair[kg,"n12"]
n01 <- wpair[kg,"n2"]-wpair[kg,"n12"]
n1. <- wpair[kg,"n1"]
n0. <- sum(wpair[,"n12"])-wpair[kg,"n1"]
n.0 <- sum(wpair[,"n12"])-wpair[kg,"n2"]
n.1 <- wpair[kg,"n2"]
ecorr <- (n11*n00 - n10*n01) / sqrt(n1.*n0.*n.0*n.1) 

# Compose matrix of beginning and ending nodes, one row per edge, beg in col 1, end in col 2
enode <- do.call(rbind, sapply(as.matrix(attr(E(x), "vnames")), function(e) strsplit(e, "\\|")))

# Plot network of word pairs (colors indicate total in-out node frequency)
png(paste("Review\\images\\OutcomeWordPairNetwork-Frequency-Class-",
          ifelse(length(ktCase)>0, names(caseType)[ktCase], "All"), "-OpType-",
          ifelse(length(ktOutcome)>0, names(opType)[ktOutcome], "All"), "-Min-Pair-Freq-", nedgethresh,
          ".png", sep=""), res=300, height=3000, width=3000)
ggraph(x, layout="fr") +
  # One plot element per edge
  geom_edge_link(#aes(#color=ifelse(wpair[kg,"n12"]<mean(wpair[kg,"n12"]), "A", "B"),
                     #width=wpair[kg,"n12"]
                     #color=wr,
                     #start_cap=circle(r=wpair[kg,"n1"]/max(wpair[kg,"n1"])),
                     #end_cap=circle(r=wpair[kg,"n2"]/max(wpair[kg,"n2"])),
                     #alpha=wpair[kg,"n12"]
                 #),
                 color="gray60",
                 #alpha=0.5,
                 #label_alpha=NA,
                 #width=1,
                 start_cap=circle(r=2.5, "mm"),
                 end_cap=circle(r=2.5, "mm"),
                 arrow=grid::arrow(type="closed", length=unit(1.5, "mm")),
                 show.legend=c(F)) +
  #scale_edge_color_manual(name="Class", values=c("A"="green", "B"="blue")) +
  #scale_edge_color_gradient(name="r", low="#2222FF", high="#FF4444") +
  #scale_edge_alpha(name="n") +
  #scale_edge_width(name="n", range=c(0, 2)) +
  # One plot element per node
  #geom_node_circle(aes(r=r1/5)) +
  #geom_node_circle(aes(r=r2/5)) +
  #geom_node_circle(aes(r=0.1, color=r1)) +
  #geom_node_point(aes(color=nfreq), size=4) +
  # Note that size parameters are strictly in units if mm
  geom_node_point(aes(color=nfreq/1000), size=5, alpha=1) +
  scale_color_gradient(name="n / 1000 words", low="#2222FF", high="#FF4444") +
  geom_node_text(aes(label=name), repel=T, point.padding=unit(1, "mm")) +
  theme_void() +
  theme(plot.margin=unit(c(0.5, 0.25, 0.5, 0.25), "in"), legend.box.spacing=unit(0.35, "in"))
dev.off()

# Plot word pair correlation (edge color indicates correlation)
png(paste("Review\\Images\\OutcomeWordPairNetwork-Correlation-Class-",
          ifelse(length(ktCase)>0, names(caseType)[ktCase], "All"), "-OpType-",
          ifelse(length(ktOutcome)>0, names(opType)[ktOutcome], "All"), "-Min-Pair-Freq-", nedgethresh,
          ".png", sep=""), res=300, height=3000, width=3000)
ggraph(x, layout="fr") +
  # One plot element per edge
  geom_edge_link(aes(color=ecorr,
                     start_cap=circle(r=2+2*(nfreq[enode[,1]]-min(nfreq[enode[,1]]))/(max(nfreq[enode[,1]])-min(nfreq[enode[,1]])), "mm"),
                     end_cap=circle(r=2+2*(nfreq[enode[,2]]-min(nfreq[enode[,2]]))/(max(nfreq[enode[,2]])-min(nfreq[enode[,2]])), "mm")),
                 width=1, arrow=grid::arrow(type="closed", length=unit(1.5, "mm"))) +
  scale_edge_color_gradient(name="edge-r", low="#2222FF", high="#FF4444") +
  # One plot element per node
  geom_node_point(aes(size=nfreq/1000), shape=1) + #, color="#DDDDDD") +
  # Size parameters are in strict mm units
  scale_size_continuous(range = c(2, 5)) +
  geom_node_text(aes(label=name), repel=T, point.padding=unit(1, "mm")) +
  theme_void() +
  theme(plot.margin=unit(c(0.5, 0.25, 0.5, 0.25), "in"), legend.box.spacing=unit(0.35, "in"))
dev.off()


####
#### igraph notes
####

help(igraph)
browseURL(file.path(path.package("igraph"), "html", "00Index.html")) 

x <- igraph::make_graph(wv)

class(x) # "igraph" although object appears and can be referenced as a list
attributes(x) # $class="igraph"
igraph::print_all(x) # IGRAPH 775bc95 DN-- 73 49 -- followed by attrs and edges
                     # D = directed, N = named (vertices?)
                     # 73 vertices, 49 edges
                     # A vector-like string of pairs of edge forming nodes is displayed,
                     # elements [1:2] form first edge, [3:4] the second, etc.

graph_attr(x, "name") <- 1:length(E(x))
graph_attr(x, "color") <- c("red", "green", "blue", rep("black", length(E(x))-3))

igraph::graph_attr_names(x) # character(0)
igraph::graph_attr(x) # named_list()
igraph::edge_attr(x) # list()
igraph::vertex_attr(x) # distinct list of node names (no edge relations implied)

# graph elements are vectors, one for each node, each of length n(node names), where
# element j of vector i contains a 1 if node(1,j) from an edge, 0 otherwise
E(x) # edges
ecount(x) # 49, edge count
class(x[3]) # "numeric"
x[3]
# attributes do not appeat to be readily useful in ggraph plots
edge_attr(x, "color", 1) <- "red"
edge_attr(x)

# Individual edge
E(x)[1]
class(E(x)[1])
attributes(E(x)[1])
class(attr(E(x)[1], "vnames"))
unlist(strsplit(attr(E(x)[1], "vnames"), "\\|"))

V(x) # vertices
length(V(x)) # 73
V(x)[1]
attributes(x[73])
vertex_attr(x, "label")
set_vertex_attr(x, "color", index=V(x)[1], "red")
vertex_attr(x, "color", index=1) <- "red"
set_vertex_attr("label", value=...)

y <- get.adjacency(x) # sparse adjacency matrix

farthest.nodes(x) # network diameter

# neighborhood
neighbors(x, 2, "all") # adjacent vertices, also "out" and "in"
i0 <- which(vertex_attr(x, "name")=="circuit")
i1 <- which(x[i0]==1)
c(V(x)[i0], V(x)[i1])
 
# number of nodes within directed paths of specified order (vector of counts for each node) 
ego_size(x, order=2, nodes=V(x), mode="out") # c("all", "out", "in")

# list (one element per node) of related nodes with paths within specified order
# filter to min oreder to identify neighborhood boundary
ego(x, order=2, nodes=V(x), mode="in", mindist = 2)

# add nodes
x <- x + vertices("X", "Y")

# add and delete edges
x <- x + edge(1, 5, color = "green") +
         edge(2, 6, color = "blue") -
         edge("8|9")



igraph.console() # requires x11




############################################################################################################
#### Plot distribution of word counts by case type and opinion type
#### Note that all "words" remaining after omission of punctuation, numerals, and special characters are
#### included in counts - some remaining strings are headings and other non-body text
############################################################################################################

# Mean words per outcome
execQuery("select sum(length(Outcome))/count(1) from CaseHeader")

# Aggregate word count by case type and opinion type
nword <- execQuery(paste(" select distinct cty.CaseType, oty.OutcomeType, ifnull(character_length(ch.Outcome), 0) as n",
                         " from   CaseHeader ch join CaseTypeComposite cty on ch.LNI=cty.LNI",
                         "        join CaseOutcomeType oty on ch.LNI=oty.LNI", sep=""))

table(nword[,"CaseType"], nword[,"OutcomeType"])

# Factor outcome types for legend appearance order
nword[,"OutcomeType"] <- factor(nword[,"OutcomeType"], levels=outcomeType)

# Density ridge plot
#png("Review\\Images\\CaseType-OutcomeType-WordCount-Density.png", res=300, width=2400, height=2400)
ggplot(data=nword) +
  geom_density_ridges(aes(x=n, y=OutcomeType, fill=OutcomeType, point_color=OutcomeType),
                      alpha=0.5, point_size=1.75, point_shape=16, jittered_points=T, point_alpha=0.5) +
  scale_x_continuous(labels=function(x) format(x, big.mark=",")) +
  scale_fill_manual(name="Opinion Type", values=setNames(c("#6ab187", "#dbae58", "#0091d5", "#dd0000", "#aaaaaa"), outcomeType)) +
  scale_discrete_manual(name="Opinion Type", aesthetics="point_color",
                        values=setNames(c("#6ab187", "#dbae58", "#0091d5", "#ee0000", "#aaaaaa"), outcomeType)) +
  facet_wrap(~CaseType, labeller=as_labeller(setNames(names(caseType), caseType))) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=12, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing.x=unit(0, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=8),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color="gray"),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)) +
  labs(title="\nLexis Nexis Appeals Outcome Text",
       subtitle="Distribution of Word Count by Case Category\n",
       x="\nWords", y="Normalized Frequency\n")
dev.off()

k <- which(nword[,"CaseType"]=="Civil" & nword[,"OutcomeType"]=="Affirmed")
hist(nword[k,"n"])
