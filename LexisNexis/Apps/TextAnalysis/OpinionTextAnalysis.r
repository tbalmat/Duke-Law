# Duke University Law LexisNexis Court Opinion Text Analysis

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(LexisNexis)
library(ggplot2)
library(igraph)
library(ggraph)
library(ggridges)
library(XML)

# Specify current working dir
setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisDecisionSample\\OpinionTextAnalysis")


#############################################################################################
#### Function:  execute query
#############################################################################################

execQuery <- function(sql) {
  library(RODBC)
  db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=LexisNexis; trusted_connection=true", readOnlyOptimize=T)
  d <- sqlQuery(db, sql, stringsAsFactors=F, as.is=T)
  odbcClose(db)
  return(d)
}


#############################################################################################
#### Case classes and opinion types
#############################################################################################

x <- execQuery("select * from CaseClass")
caseClass <- setNames(x[,1], x[,2])
x <- execQuery("select * from OpinionType")
opType <- setNames(x[,1], x[,2])


#############################################################################################
#### Construct list of key words, either to be omitted from or identified in text
#############################################################################################

pronouns <- tolower(scan("Pronouns.csv", what="character"))
omitWords <- tolower(scan("OmitWords.csv", what="character"))
latinLegal <- tolower(xmlToDataFrame("LatinLegalTerms.xml")[,1])
matchWords <- tolower(scan("Words-1.csv", what="character"))

###############################################################################################
#### Function:
#### Query opinion text and split into vector of words
#### Omit punctuation, numerals, pronouns, and words of length less than three
#### Convert to lower case
###############################################################################################

opWords <- function(caseClass, opType, omitWords, lengthThresh, omitNonAlpha, caseID=0) {

  opText <- execQuery(paste(" select od.opinionText",
                            " from   CaseHeader ch join OpinionHeader oh on ch.ID=oh.CaseID",
                            "        join OpinionDetail od on oh.CaseID=od.CaseID",
                            "        and oh.OpinionID=od.OpinionID",
                            " where  1=1",
                            ifelse(caseClass!="", paste(" and ch.Class='", caseClass, "'", sep=""), ""),
                            ifelse(opType!="", paste(" and oh.Type='", opType, "'", sep=""), ""),
                            ifelse(caseID>0, paste(" and ch.ID=", caseID, sep=""), ""),  sep=""))[,1]
    # Split words
    # Omit punctuation, special symbols, and numerals, if requested
    w <- unlist(lapply(unlist(strsplit(tolower(opText), " ")),
                  function(a)
                    if(omitNonAlpha) {
                      gsub("[^a-z]", "", a)
                    } else {
                      a
                    }))
    # Omit words in specified list or with length <= specified threshold
    k <- which(nchar(w)>lengthThresh & !w %in% omitWords)
    return(w[k])

}


###############################################################################################
#### Plot distribution of Latin legal terms by case class
###############################################################################################

# Aggregate Latin term frequency by class
ldat <- do.call(rbind, apply(as.matrix(caseClass), 1,
                         function(cl) {
                           # Retrieve text for class
                           txt <- tolower(
                                    execQuery(
                                      paste("select od.opinionText ",
                                            "from   CaseHeader ch join OpinionDetail od on ch.ID=od.CaseID ",
                                            "where  ch.Class='", cl, "'", sep=""))[,1])
                           # Identify terms
                           # Note that grep() returns a vector of text array indices that match
                           # If an array element conatins multiple instances of a searched text, it is
                           # counted once only
                           ltn <- apply(as.matrix(latinLegal), 1, function(lt) length(grep(lt, txt)))
                           k <- which(ltn>0)
                           data.frame("caseClass"=cl, "term"=latinLegal[k], "n"=ltn[k])
                         }))

# Limit to terms with a minimum specified frequency
nmin <- 5
k <- which(ldat[,"term"] %in% ldat[which(ldat[,"n"]>=nmin),"term"] & !ldat[,"term"] %in% c("r", "i.e."))

png("Review\\Images\\LatinLegalTermByClass.png", res=300, height=2400, width=2400)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=n), stat="identity", fill="blue3") +
  facet_wrap(~caseClass) +
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle=paste("Distribution of Latin Legal Terms by Case Class\nTerms with at least ",
                      nmin, " Instances within at least One Class\n", sep=""),
       x="term", y="frequency")
dev.off()


###############################################################################################
#### Plot distribution of Latin legal terms by opinion type
###############################################################################################

# List opinion types
type <- execQuery("select distinct type from OpinionHeader")[,1]

# Aggregate Latin term frequency by type
ldat <- do.call(rbind, apply(as.matrix(type), 1,
                         function(type) {
                           # Retrieve text for type
                           txt <- tolower(
                                    execQuery(
                                      paste("select od.opinionText ",
                                            "from   OpinionHeader oh join OpinionDetail od on oh.CaseID=od.CaseID ",
                                            "       and oh.OpinionID=od.OpinionID ",
                                            "where  oh.Type='", type, "'", sep=""))[,1])
                           # Identify terms
                           # Note that grep() returns a vector of text array indices that match
                           # If an array element conatins multiple instances of a searched text, it is
                           # counted once only
                           ltn <- apply(as.matrix(latinLegal), 1, function(lt) length(grep(lt, txt)))
                           k <- which(ltn>0)
                           data.frame("type"=type, "term"=latinLegal[k], "n"=ltn[k])
                         }))

# Limit to terms with a minimum specified frequency
nmin <- 5
k <- which(ldat[,"term"] %in% ldat[which(ldat[,"n"]>=nmin),"term"] & !ldat[,"term"] %in% c("r", "i.e."))

png("Review\\Images\\LatinLegalTermByOpType.png", res=300, height=2400, width=3000)
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle=paste("Distribution of Latin Legal Terms by Opinion Type\nTerms with at least ",
                      nmin, " Instances within at least One Op Type\n", sep=""),
       x="term", y="frequency")
dev.off()


###############################################################################################
#### Plot frequencies of top occurring words by case class
###############################################################################################

# Aggregate word frequency by class
ldat <- do.call(rbind, apply(as.matrix(caseClass), 1,
                         function(cl) {
                           w <- opWords(caseClass=cl, opType="", omitWords=c(pronouns, omitWords),
                                        lengthThresh=2, omitNonAlpha=T)
                           wag <- aggregate(rep(1, length(w)), by=list(w), sum)
                           data.frame("caseClass"=cl, "term"=wag[,1], "n"=wag[,2])
                         }))

# Convert to proportions within class
nclass <- aggregate(ldat[,"n"], by=list(ldat[,"caseClass"]), sum)
colnames(nclass) <- c("caseClass", "n")
rownames(nclass) <- nclass[,"caseClass"]
ldat[,"p"] <- ldat[,"n"]/nclass[ldat[,"caseClass"],"n"]

# Limit to top occurring terms
ntop <- 20
k <- which(ldat[,"term"] %in% unique(ldat[order(ldat[,"p"], decreasing=T),"term"])[1:ntop])

png("Review\\Images\\WordDistributionByClass.png", res=300, height=2400, width=2400)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=p), stat="identity", fill="blue3") +
  facet_wrap(~caseClass, labeller=as_labeller(setNames(names(caseClass), caseClass))) +
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle=paste("Distribution of Top ", ntop, " Words by Case Category\n", sep=""),
       x="\nword", y="proportion\n")
dev.off()


###############################################################################################
#### Plot frequencies of top occurring words by opinion type
###############################################################################################

# Aggregate word frequency by op type
ldat <- do.call(rbind, apply(as.matrix(opType), 1,
                         function(op) {
                           w <- opWords(caseClass="", opType=op, omitWords=c(pronouns, omitWords),
                                        lengthThresh=2, omitNonAlpha=T)
                           wag <- aggregate(rep(1, length(w)), by=list(w), sum)
                           data.frame("opType"=op, "term"=wag[,1], "n"=wag[,2])
                         }))

# Convert to proportions within class
nop <- aggregate(ldat[,"n"], by=list(ldat[,"opType"]), sum)
colnames(nop) <- c("opType", "n")
rownames(nop) <- nop[,"opType"]
ldat[,"p"] <- ldat[,"n"]/nop[ldat[,"opType"],"n"]

# Limit to top occurring terms
ntop <- 50
k <- which(ldat[,"term"] %in% unique(ldat[order(ldat[,"p"], decreasing=T),"term"])[1:ntop])

png("Review\\Images\\WordDistributionByOpType.png", res=300, height=3000, width=2400)
ggplot() +
  geom_bar(data=ldat[k,], aes(x=term, y=p), stat="identity", fill="blue3") +
  facet_wrap(~opType, ncol=1, labeller=as_labeller(setNames(names(opType), opType))) +
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle=paste("Distribution of Top ", ntop, " Words by Opinion Type\n", sep=""),
       x="\nword", y="proportion\n")
dev.off()


###############################################################################################
#### Plot word proportions of one class against another 
###############################################################################################

qclass <- c("CrimPun", "Other")  #  "Comm", "CrimPun", "InsInj", "Other", "Prop"  

# Aggregate proportions for both classes
w <- opWords(caseClass=qclass[1], opType="", omitWords=c(pronouns, omitWords), lengthThresh=2, omitNonAlpha=T)
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
w <- opWords(caseClass=qclass[2], opType="", omitWords=c(pronouns, omitWords), lengthThresh=2, omitNonAlpha=T)
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

# Join by word
w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
colnames(w) <- c("term", "p1", "p2")

# Convert NA to 0
w[which(is.na(w[,"p1"])),"p1"] <- 0
w[which(is.na(w[,"p2"])),"p2"] <- 0

# Filter by p
pmin <- 0.002
k <- which(w[,"p1"]>=pmin | w[,"p2"]>=pmin)

# Set y offset for text
tyoffset <- 0.00025

png(paste("Review\\Images\\WordDistribution-", qclass[2], "-by-", qclass[1], ".png", sep=""), res=300, height=2400, width=2400)
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
  labs(title="Lexis Nexis Case Opinion Samples (212 cases)",
       subtitle=paste("Pairwise Word Proportions for Case Classes ", qclass[2], " and ", qclass[1], "\n", sep=""),
       x=qclass[1], y=class[2])
dev.off()


###############################################################################################
#### Plot word proportions of one opinion type against another 
###############################################################################################

qtype <- c("concur", "majority")  #   "concur", "dissent", "majority"

# Aggregate proportions for both types
w <- opWords(caseClass="", opType=qtype[1], omitWords=c(pronouns, omitWords),
             lengthThresh=2, omitNonAlpha=T))
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
w <- opWords(caseClass="", opType=qtype[2], omitWords=c(pronouns, omitWords),
             lengthThresh=2, omitNonAlpha=T))
wag <- aggregate(rep(1, length(w)), by=list(w), sum)
w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

# Join by word
w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
colnames(w) <- c("term", "p1", "p2")

# Convert NA to 0
w[which(is.na(w[,"p1"])),"p1"] <- 0
w[which(is.na(w[,"p2"])),"p2"] <- 0

# Filter by p
pmin <- 0.001
k <- which(w[,"p1"]>=pmin | w[,"p2"]>=pmin)

# Set y offset for text
tyoffset <- 0.0002

# Set axis limits
axlim <- 0.01

png(paste("Review\\Images\\WordDistribution-", type[2], "-by-", type[1], "-b.png", sep=""), res=300, height=2400, width=2400)
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle=paste("Pairwise Word Proportions for Opinion Types ", type[2], " and ", type[1], "\n", sep=""),
       x=type[1], y=type[2])
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

# Select case class, opinion type, and minimum edge frequency for inclusion 
kclass <- 2
kop <- 3
nedgethresh <- 40

# Retrieve opinion paragraphs
opText <- execQuery(paste(" select od.opinionText",
                          " from   CaseHeader ch join OpinionHeader oh on ch.ID=oh.CaseID",
                          "        join OpinionDetail od on oh.CaseID=od.CaseID",
                          "        and oh.OpinionID=od.OpinionID",
                          " where  1=1",
                          ifelse(length(kclass)>0, paste(" and ch.Class='", caseClass[kclass], "'", sep=""), ""),
                          ifelse(length(kop)>0, paste(" and oh.Type='", opType[kop], "'", sep=""), ""), sep=""))[,1]

# Split each paragraph into words
# Omit punctuation, special symbols, numerals, pronouns, and words of length < three
# Compose pairs of consecutive words 
w <- do.call(rbind,
             sapply(opText,
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

# Join to aggregated counts of leading and trainling word appearance
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
png(paste("Review\\images\\WordPairNetwork-Frequency-Class-",
          ifelse(length(kclass)>0, names(caseClass)[kclass], "All"), "-OpType-",
          ifelse(length(kop)>0, names(opType)[kop], "All"), "-Min-Pair-Freq-", nedgethresh,
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
png(paste("Review\\Images\\WordPairNetwork-Correlation-Class-",
          ifelse(length(kclass)>0, names(caseClass)[kclass], "All"), "-OpType-",
          ifelse(length(kop)>0, names(opType)[kop], "All"), "-Min-Pair-Freq-", nedgethresh,
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
#### Plot distribution of word counts by case class and opinion type
#### Note that all "words" remaining after omission of punctuation, numerals, and special characters are
#### included in counts - some remaining strings are headings and other non-body text
############################################################################################################

# Mean words per opinion
nop <- execQuery("select count(1) from OpinionHeader")[,1]
nword <- length(opWords(caseClass="", opType="", omitWords="", lengthThresh=0, omitNonAlpha=T, caseID=0))
nword/nop

# Aggregate word count by case, class, and opinion type
# Assemble case IDs by class and opinion type
idl <- apply(execQuery(paste("select distinct ch.Class, oh.Type",
                             " from  CaseHeader ch join OpinionHeader oh on ch.ID=oh.CaseID", sep="")), 1,
             function(clop) list("caseClass"=clop[1], "opType"=clop[2], 
                                 "ID"=execQuery(paste("select ch.ID",
                                                      " from  CaseHeader ch join OpinionHeader oh on ch.ID=oh.CaseID",
                                                      " where ch.Class='", clop[1], "' and oh.Type='", clop[2], "'",
                                                      sep=""))[,"ID"]))

# Aggregate word count by case class and opinion type
nword <- do.call(rbind,
           lapply(idl,
             function(idl)
               data.frame("caseClass"=idl[["caseClass"]], "opType"=idl[["opType"]], "caseID"=idl[["ID"]],
                          "nword"=apply(as.matrix(idl[["ID"]]), 1,
                                    function(cid)
                                      length(opWords(caseClass=idl[["caseClass"]], opType=idl[["opType"]],
                                                     omitWords="", lengthThresh=0, omitNonAlpha=T, caseID=cid))),
                          row.names=1:length(idl[["ID"]])) ))

# Construct smoothed nword histogram frequencies
# Configure histogram breaks using max observed word count
# For uniformity of x axis, these will be used for all case classes and op types
bins <- 30
# Span setting specifies the proportion of points local to each point used in fitting polynomials
# Greater than 1=1, small becomes ragged and approaches histogram-like dist
# Also, small values of span (low proportion points) may result in too few points remaining to
# satisfy polynomial regression degrees of freedom reqs
lospan <- 0.5
brk <- seq(0, max(nword[,"nword"])+1, max(nword[,"nword"])/bins)
hword <- do.call(rbind,
           lapply(idl,
             function(idl) {
               # Subset observations for current case class and opinion type
               k <- which(nword[,"caseClass"]==idl[["caseClass"]] & nword[,"opType"]==idl[["opType"]])
               # Construct frequency counts
               h <- hist(nword[k,"nword"], breaks=brk, plot=F)
               # Prepend a count of 0 and normalize counts to a proportion of opinions in case class
               # This enables comparison of uniform distributions by class
               y <- c(0, h[["counts"]])/length(which(nword[,"caseClass"]==idl[["caseClass"]]))
               # Smooth frequencies and convert negative values to 0
               y <- loess(y~brk, span=lospan, degree=2)[["fitted"]]
               y[which(y<0)] <- 0
               # Return smoothed frequencies
               data.frame("caseClass"=idl[["caseClass"]], "opType"=idl[["opType"]],
                          "nword"=brk, "nop"=y,
                           row.names=1:length(brk))
             }))
             
# Plot
png("Review\\Images\\CaseClass-OpinionType-WordCount-Distribution.png", res=300, width=2400, height=2400)
ggplot() +
  # Histograms do not render correct proportions of op type within case class
  #geom_histogram(data=nword, aes(x=nword, y=..count.., fill=opType), bins=10, position="dodge", color="white") +
  #geom_histogram(data=nword, aes(x=nword, y=..ncount..), bins=20, position="stack") +
  # Proportions incorrect here also - it appears that each class-type category has distribution 1.0 and the
  # graph simply shows where density is (we need proportion of opinions within case class, colored by type)
  #geom_freqpoly(data=nword, aes(x=nword, y=..ncount.., color=opType), bins=30, closed="left", position="stack") +
  # Unable to smooth, plus PROPORTION OPINIONS WITHIN CASE TYPE IS WRONG (compare to computed area plot)
  #geom_area(data=nword, aes(x=nword, y=..ncount.., fill=opType), stat="bin", bins=4, position="stack") +
  # Other interesting options that do not offer quite what we need
  # Note the computed values (..count.., ..density..) made available by stat="bin" and others
  #stat_smooth(data=nword, aes(x=nword, y=..y.., fill=opType), geom="area") +
  #stat_density(data=nword, aes(x=nword, y=..scaled.., fill=opType), kernel="triangular", n=256, position="stack") +
  #
  # Area using computed, smoothed histogram frequencies
  geom_area(data=hword, aes(x=nword, y=nop, fill=opType), position="stack") +
  scale_x_continuous(labels=function(x) format(x, big.mark=",")) +
  scale_fill_manual(name="Opinion Type", values=c("concur"="#6ab187", "dissent"="#dbae58", "majority"="#0091d5")) +
  facet_wrap(~caseClass, labeller=as_labeller(setNames(names(caseClass), caseClass))) +
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle="Distribution of Word Count by Case Category\n",
       x="\nWords", y="Normalized Frequency\n")
dev.off()

table(nword[["caseClass"]], nword[["opType"]])

# Density ridge plot
png("Review\\Images\\CaseClass-OpinionType-WordCount-Density.png", res=300, width=2400, height=2400)
ggplot(data=nword) +
  geom_density_ridges(aes(x=nword, y=opType, fill=opType, point_color=opType),
                      alpha=0.5, point_size=1.75, point_shape=16, jittered_points=T, point_alpha=0.75) +
  #scale_point_color_hue(name="Opinion Type", l=50) +
  scale_x_continuous(labels=function(x) format(x, big.mark=",")) +
  scale_fill_manual(name="Opinion Type", values=c("concur"="#6ab187", "dissent"="#dbae58", "majority"="#0091d5")) +
  scale_discrete_manual(name="Opinion Type", aesthetics="point_color",
                        values=c("concur"="#6ab187", "dissent"="#dbae58", "majority"="#0091d5")) +
  #scale_color_manual(name="Opinion Type", values=c("concur"="#6ab187", "dissent"="#dbae58", "majority"="#0091d5")) +
  facet_wrap(~caseClass, labeller=as_labeller(setNames(names(caseClass), caseClass))) +
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
  labs(title="\nLexis Nexis Case Opinion Samples (212 cases)",
       subtitle="Distribution of Word Count by Case Category\n",
       x="\nWords", y="Normalized Frequency\n")
dev.off()
