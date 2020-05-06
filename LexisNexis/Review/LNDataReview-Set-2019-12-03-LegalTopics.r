# Duke University Law Appeals Analysis
# Review of 2019-12-03 LexisNexis Data
# Legal Topics

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(ggplot2)
library(xtable)
library(DBI)
library(RMySQL)

#######################################################################################
#### Set directories
#######################################################################################

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-02-21")
lnsourcedir1 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-03-13"
lnsourcedir2 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03"
imgdir <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-02-21"


#######################################################################################
#### Connect to Appeals databases
#### db1 for data set 1, March 2019
#### db2 for data set 2, December 2019
#######################################################################################

usr <- "tjb48"
db1 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals", user=usr, password=rstudioapi::askForPassword("Password:  "))
db2 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals2", user=usr, password=rstudioapi::askForPassword("Password:  "))


#######################################################################################
#######################################################################################
#### SQL data review
#######################################################################################
#######################################################################################


#######################################################################################
#### List table structures
#######################################################################################

dbGetQuery(db1, "show tables")
dbGetQuery(db2, "show tables")
dbGetQuery(db1, "describe CaseHeader")
dbGetQuery(db2, "describe CaseHeader")
dbGetQuery(db1, "describe Court")
dbGetQuery(db2, "describe Court")


#######################################################################################
#### Review cases with no legal topics
#######################################################################################

db <- db1

dbGetQuery(db,
  "select count(1) as n, sum(case when(LNI not in(select distinct LNI from CaseLegalTopics))then 1 else 0 end) as n0
   from   CaseHeader")

# List sample of cases with no legal topics
y <- sample(x[,1], 100, replace=F)
z <- dbGetQuery(db,
       paste(" select LNI, CaseTitleLexisNexis",
             " from   CaseHeader",
             " where  LNI in('", paste(y, collapse="', '", sep=""), "')", sep=""))

# Render in Latex
a <- ""
for(i in 1:nrow(z))
  a <- c(a, paste(z[i,1], " & ", gsub("&", "\\\\&", z[i,2]), "\\\\", sep=""))
writeLines(a)


#######################################################################################
#### Tabulate number of legal topics by year
#######################################################################################

# Append data frames of case counts by year, one from each data set
x <- do.call(rbind,
       lapply(c("A", "B"),
         function(s) {
           if(s=="A") {
             db <- db1
           } else {
             db <- db2
           }
           data.frame("set"=s,
                      dbGetQuery(db,
                        "select   year(a.DecisionDate) as yr, count(1) as n
                         from     CaseHeader a join CaseLegalTopics b on a.LNI=b.LNI
                         group by year(a.DecisionDate)"))
         }))

# Reorder data sets so that March appears first in legends
x[,"set"] <- factor(x[,"set"], levels=c("A", "B"))

# Review min and max years by data set
dbGetQuery(db1, "select min(year(DecisionDate)), max(year(DecisionDate)) from CaseHeader")
dbGetQuery(db2, "select min(year(DecisionDate)), max(year(DecisionDate)) from CaseHeader")

# Save overlayed area plot
# Position="identity" aesthetics causes overlay instead of the default stacking
png(paste(imgdir, "\\LegalTopics\\images\\DistributionNumberOfTopicsByYear.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_area(data=x, aes(x=yr, y=n, fill=set, group=set), alpha=0.35, position="identity") +
  scale_fill_manual(values=c("A"="red", "B"="blue")) +
  scale_x_continuous(breaks=seq(1974, 2018, 2)) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        #panel.spacing=unit(-0.2, "lines"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="\nyear", y="number of topics assigned\n")
dev.off()


#######################################################################################
#### Tabulate number of legal topics per case
#######################################################################################

# Append data frames of topic counts by case, one from each data set
x <- do.call(rbind,
       lapply(c("A", "B"),
         function(s) {
           if(s=="A") {
             db <- db1
           } else {
             db <- db2
           }
           data.frame("set"=s,
                      dbGetQuery(db,
                        "select   case when(b.n is not null)then b.n else 0 end as nTopic, count(1) as nCase
                         from     CaseHeader a
                                  left join(select   LNI, count(1) as n
                                            from     CaseLegalTopics
                                            group by LNI) b on a.LNI=b.LNI
                         group by b.n"))
         }))

# Reorder data sets for legend appearance
x[,"set"] <- factor(x[,"set"], levels=c("A", "B"))

# Generate plot with one line per data set
png(paste(imgdir, "\\LegalTopics\\images\\DistributionNumberOfTopicsPerCase.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_line(data=x, aes(x=nTopic, y=log(nCase)/log(10), linetype=set)) +
  scale_y_continuous(breaks=1:6, labels=format(10**(1:6), big.mark=",")) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        #panel.spacing=unit(-0.2, "lines"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="\nnumber of topics assigned", y="number of cases\n")
dev.off()


#######################################################################################
#### Evaluate case legal topic LNIs
#######################################################################################

# LNIs have format XXXX-XXXX-XXXX-XXXX-00000-0X, where each X is either a
# single integer (0-9) or upper case character (A-Z)

# Select data set
db <- db2

# Identify LNIs that do not conform to the official pattern
# The following grep pattern is taken from a review of the March data
x <- dbGetQuery(db,
       paste(" select LNI",
             " from   CaseLegalTopics",
             " where  not LNI regexp '[34578][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                                     "[0123456789BCDFGHJKMNPRSTVWXY]{3}[01]-",
                                     "[026DFJKTY][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                                     "[0123456789BCDFGHJKMNPRSTVWXY][0-5][0123456789BCDFGHJKMNPRSTVWXY]{2}-",
                                     "00000-0[0-5]'", sep=""))[,"LNI"]

# Enumerate non-conforming LNIs
length(x)

# List, by each position the set of characters appearing in that position
lapply(1:28, function(i) paste(sort(unique(substring(x, i, i))), collapse=""))

# Sample non-conforming LNIs
x[1:100,]


#######################################################################################
#### Evaluate case legal topic existence and uniqueness
#######################################################################################

# Select data set
db <- db1

# Retrieve unknown IDs
x <- dbGetQuery(db, "select   TopicID, char_length(TopicID), count(1)
                     from     CaseLegalTopics
                     where    TopicID not in(select ID from Appeals2.LegalTopics)
                     group by TopicID, char_length(TopicID)")

# Enumerate
nrow(x)

# Render in Latex
a <- ""
for(i in 1:nrow(x))
  a <- c(a, paste(x[i,1], " & ", x[i,2], " & ", x[i,3], "\\\\", sep=""))
writeLines(a)

# Enumerate the set of all topics
dbGetQuery(db, "select count(1) from CaseLegalTopics")

# Identify non-unique topics by LNI
# Note that the import procedure eliminated duplicate topics by LNI, so the following
# query should always produce a null set
dbGetQuery(db, "select   LNI, TopicID, count(1) as n
                from     CaseLegalTopics
                group by LNI, TopicID
                having   count(1)>1")


#######################################################################################
#### Evaluate case legal topic guids
#######################################################################################

# Inspection of Topic IDs reveals that they are guids of length 32, with each position
# containing a character encoded hex digit (0 through F)

db <- db2

# Evaluate length(s) of legal topics in master list (LegalTopics table)
dbGetQuery(db, "select char_length(ID), count(1) from LegalTopics group by char_length(ID)")

# Identify topic IDs in the master list that do not have a valid guid format
# The following grep pattern represents a 32 bit hex integer
x <- dbGetQuery(db,
       paste(" select ID",
             " from   LegalTopics",
             " where  not ID regexp '[0123456789ABCDEF]{32}'", sep=""))[,"ID"]
length(x)

# Check for case topics with no case reference (there should be none since all topics were
# extracted from case records in the source, and each case record contained an LNI value 
dbGetQuery(db, "select count(1) from CaseLegalTopics where LNI is null")

# Enumerate unique legal topics codes used on cases
dbGetQuery(db, "select count(distinct TopicID) from CaseLegalTopics")

# Evaluate length(s) of legal topics appearing on cases
dbGetQuery(db, "select char_length(TopicID), count(1) from CaseLegalTopics group by char_length(TopicID)")

# Retrieve case legal topics with improperly formatted guids
# The following grep pattern represents a 32 bit hex integer
x <- dbGetQuery(db1,
       paste(" select TopicID",
             " from   CaseLegalTopics",
             " where  not TopicID regexp '[0123456789ABCDEF]{32}'", sep=""))[,"TopicID"]

lapply(1:32, function(i) paste(sort(unique(substring(x, i, i))), collapse=""))


#######################################################################################
#### Distribution of the difference in number of cases by legal topic
#######################################################################################

# Retrieve March and December frequenciew by topic
x <- dbGetQuery(db1, "select TopicID, count(1) from CaseLegalTopics group by TopicID")
y <- dbGetQuery(db2, "select TopicID, count(1) from CaseLegalTopics group by TopicID")
nrow(x)
nrow(y)

# Merge March and Decdmber counts by topic
# Retain topics that do not appear in the alternate dat set
z <- merge(x, y, by="TopicID", all.x=T, all.y=T)
colnames(z) <- c("TopicID", "n1", "n2")

# Convert counts to 0 for topics missing in one data set
z[1:100,]
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0

# Compute the difference in counts, between data sets, by topic
z[,"nDiff"] <- z[,"n2"]-z[,"n1"]

# Inspect maximum frequencies
max(z[,"n1"])
max(z[,"n2"])

# List topics with maximum frequency
z[which(z[,"n1"]==max(z[,"n1"])),]
z[which(z[,"n2"]==max(z[,"n2"])),]

# Retrieve individual topic for review
dbGetQuery(db2, "select * from LegalTopics where ID='E8C8F7EBA70044919582766BE13D3B1B'")

# Plot a histogram of the difference in counts, between sets, by topic
png(paste(imgdir, "\\LegalTopics\\images\\DistributionDifferenceInCasesByTopic.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_histogram(data=z, aes(x=nDiff), color="gray85", fill="blue3") +
  scale_x_continuous(limits=c(-2500, 500), labels=function(x) format(x, big.mark=",")) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        #panel.spacing=unit(-0.2, "lines"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="\ndifference in number of cases assigned by topic", y="number of cases\n")
dev.off()


#######################################################################################
#### Distribution of the difference in number of legal topics by case
#######################################################################################

# Retrieve March and December topic counts by case
x <- dbGetQuery(db1, "select LNI, count(1) from CaseLegalTopics group by LNI")
y <- dbGetQuery(db2, "select LNI, count(1) from CaseLegalTopics group by LNI")
nrow(x)
nrow(y)

# Merge March and Decdmber counts by case
# Retain cases that do not appear in the alternate dat set
z <- merge(x, y, by="LNI", all.x=T, all.y=T)
colnames(z) <- c("LNI", "n1", "n2")

# Convert counts to 0 for cases missing in one data set
z[1:100,]
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0

# Compute the difference in counts, between data sets, by case
z[,"nDiff"] <- z[,"n2"]-z[,"n1"]

# Inspect maximum frequencies
max(z[,"n1"])
max(z[,"n2"])

# Plot a histogram of the difference in counts, between sets, by case
png(paste(imgdir, "\\LegalTopics\\images\\DistributionDifferenceInTopicsByCase.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_histogram(data=z, aes(x=nDiff), color="gray85", fill="blue3") +
  #scale_x_continuous(limits=c(-2500, 500), labels=function(x) format(x, big.mark=",")) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        #panel.spacing=unit(-0.2, "lines"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="\ndifference in number of topics by case", y="number of cases\n")
dev.off()


#######################################################################################
#### Compute, by year:
#### The proportion of case/topic combinations that appear in both data sets
#### The proportion of case/topic combinations that appear in March, but not December
#### The proportion of case/topic combinations that appear in December, but not March
#######################################################################################

# Review total topic assignment count
dbGetQuery(db1, "select count(1) from Appeals.CaseLegalTopics")
dbGetQuery(db2, "select count(1) from Appeals2.CaseLegalTopics")

# Identify case/topic combinations missing in December
# MySQL does not support full joins, so evaluate directions separately
suppressWarnings(
  dbGetQuery(db1, "select sum(case when(t2.LNI is not null)then 1 else 0 end) as exists12,
                          sum(case when(t2.LNI is null)then 1 else 0 end) as missing2
                   from   Appeals.CaseLegalTopics t1 left join Appeals2.CaseLegalTopics t2
                          on t1.LNI=t2.LNI and t1.TopicID=t2.TopicID"))

# Identify case/topic combinations missing in March
# MySQL does not support full joins, so evaluate directions separately
suppressWarnings(
  dbGetQuery(db2, "select count(1),
                          sum(case when(t2.LNI is not null)then 1 else 0 end) as exists12,
                          sum(case when(t2.LNI is null)then 1 else 0 end) as missing1
                   from   CaseLegalTopics t1 left join Appeals2.CaseLegalTopics t2
                          on t1.LNI=t2.LNI and t1.TopicID=t2.TopicID"))

# Accumulate frequency of matching and missing legal topics by year as a proportion of total annual cases
# MySQL does not support full joins, so evaluate directions with individual queries
x <- rbind(
       dbGetQuery(db1, "select   'exist12' as dir, year(a.DecisionDate) as year,
                                 count(1) as ntopics,
                                 sum(case when(c.LNI is not null)then 1 else 0 end) as neval,
                                 sum(case when(c.LNI is not null)then 1 else 0 end)*1./count(1) as p
                        from     CaseHeader a join CaseLegalTopics b on a.LNI=b.LNI
                                 left join Appeals2.CaseLegalTopics c on b.LNI=c.LNI and b.TopicID=c.TopicID
                        group by year(a.DecisionDate)"),
       dbGetQuery(db1, "select   'missing2' as dir, year(a.DecisionDate) as year,
                                 count(1) as ntopics,
                                 sum(case when(c.LNI is null)then 1 else 0 end) as neval,
                                 sum(case when(c.LNI is null)then 1 else 0 end)*1./count(1) as p
                        from     CaseHeader a join CaseLegalTopics b on a.LNI=b.LNI
                                 left join Appeals2.CaseLegalTopics c on b.LNI=c.LNI and b.TopicID=c.TopicID
                        group by year(a.DecisionDate)"),
       dbGetQuery(db2, "select   'missing1' as dir, year(a.DecisionDate) as year,
                                 count(1) as ntopics,
                                 sum(case when(c.LNI is null)then 1 else 0 end) as neval,
                                 sum(case when(c.LNI is null)then 1 else 0 end)*1./count(1) as p
                        from     CaseHeader a join CaseLegalTopics b on a.LNI=b.LNI
                                 left join Appeals.CaseLegalTopics c on b.LNI=c.LNI and b.TopicID=c.TopicID
                        group by year(a.DecisionDate)"))

# Review the total number of case topics and the number found in both sets or missing in one set
cbind(sum(x[which(x[,"dir"]=="exist12"),"ntopics"]), sum(x[which(x[,"dir"]=="exist12"),"neval"]))
cbind(sum(x[which(x[,"dir"]=="missing1"),"ntopics"]), sum(x[which(x[,"dir"]=="missing1"),"neval"]))
cbind(sum(x[which(x[,"dir"]=="missing2"),"ntopics"]), sum(x[which(x[,"dir"]=="missing2"),"neval"]))

# Generate area plot of proportions found in both data sets or missing in one set
png(paste(imgdir, "\\LegalTopics\\images\\AnnualProportionTopicsAppearingMissingByCase.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_area(data=x, aes(x=year, y=p, fill=dir, group=dir), position="identity", alpha=0.35) +
  scale_fill_manual(name="", values=c("exist12"="green", "missing1"="blue", "missing2"="Red")) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        #panel.spacing=unit(-0.2, "lines"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="year", y="proportion of topics\n")
dev.off()


#######################################################################################
#######################################################################################
#### Source data review
#######################################################################################
#######################################################################################

#######################################################################################
#### Verify accurate import of case legal topic IDs
#######################################################################################

# Select a data set (1 for March, 2 for December)
set <- 2

# Select a database (for source to SQL comparison)
if(set==1) {
  db <- db1
} else {
  db <- db2
}
lnsourcedir <- c(lnsourcedir1, lnsourcedir2)[set]

# Note that topic guids appear in the legaltopics column in the March data, but in the legaltopicspguids column in December
tpcol <- c("legaltopics", "legaltopicpguids")[set]

# Validate a sample import of case legal topics (i indicates the source file number)
nsamp <- 100
i <- 11

# Read source file
y <- read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir, "DukePart", i, ".csv.gz")), header=T, sep=",",
                quote="\"", comment="", strip.white=T)

# Review a sample of source records
colnames(y)
y[1:10,tpcol]

# Parse topic guids from sample of source records
# Note that December guids contain "urn:topic" in their text, March guids do not
z <- do.call(rbind,
       lapply(sample(which(nchar(y[,tpcol])>0), nsamp, replace=F),
         function(j) {
           data.frame("lni"=y[j,"lni"],
                      "legaltopicid"=strsplit(gsub('urn:topic:', '', y[j,tpcol]), "\\|")[[1]])
         }))

# Compare parsed topic guids to those that were imported into the selected database
z2 <- do.call(rbind,
        apply(as.matrix(1:nrow(z)), 1,
          function(j) {
            z3 <- dbGetQuery(db, paste("select count(1) as n from CaseLegalTopics where LNI='", z[j,"lni"],
                                       "' and TopicID='", z[j,"legaltopicid"], "'", sep=""))
            data.frame("lni"=z[j,"lni"], "legaltopicid"=z[j,"legaltopicid"], "n"=z3[,"n"])
          }))

# List any LNI, topic ID combinations from source data that do not have a single db entry
z2[which(z2[,"n"]!=1),]


#######################################################################################
#### Visually compare source and parsed, saved topic IDs
#######################################################################################

# In years 1975-1980, most topic IDs appearing in set 1 appear to be absent in set 2

# Retrieve all of set 1 topics
x <- do.call(rbind,
  lapply(0:10, function(i)
                 read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")), header=T, sep=",",
                            quote="\"", comment="", strip.white=T)))[,c("lni", "date", "legaltopics")]

# Retrieve all of set 2 topics
y <- do.call(rbind,
  lapply(0:11, function(i)
                 read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")), header=T, sep=",",
                            quote="\"", comment="", strip.white=T)))[,c("lni", "date", "legaltopics", "legaltopicpguids")]

# Sample LNIs from set 1 and compare topics with same LNI in set 2
yr <- 2010
nsamp <- 100
k <- sample(which(as.integer(substring(x[,"date"], 1, 4))==yr), nsamp, replace=F)
z <- do.call(rbind, lapply(k,
                      function(k) {
                        k2 <- which(y[,"lni"]==x[k,"lni"])
                        if(length(k2)>0) {
                          return(cbind(x[k,"lni"], x[k,"legaltopics"], y[k2,"legaltopicpguids"]))
                        } else {
                          return(cbind(x[k,"lni"], x[k,"legaltopics"], "missing LNI"))
                        }
                      }))

# Spot check, include legal topic descriptions
lni <- c("3S4X-4350-0039-M2XX-00000-00", "3S4X-2D00-0039-M312-00000-00")[2]
x[which(x[,"lni"]==lni),]
y[which(y[,"lni"]==lni),]


#######################################################################################
#### List years contained in each source file
#######################################################################################

# Retrieve unique years appearing in the March data
x <- lapply(0:10,
       function(i)
         sort(unique(as.integer(substring(
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,"date"], 1, 4)))))

# Retrieve unique years appearing in the December data
y <- lapply(0:11,
       function(i)
         sort(unique(as.integer(substring(
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,"date"], 1, 4)))))

# Identify any year missing in one data set ot the other
lapply(1:10, function(i) setdiff(x[[i]], y[[i]]))


#######################################################################################
#### List, by LNI, source files and number of topics
#######################################################################################

# Retrieve counts of legal topics by case in the March data
# Note that topics may be repeated by case (many instances have been observed)
# Each duplication is counted here
x <- do.call(rbind,
       lapply(0:10,
         function(i) {
           f <- sprintf("%s%02.0f", "DukePart", i)
           z <- read.table(gzfile(paste(lnsourcedir1, "\\", f, ".csv.gz", sep="")), header=T, sep=",",
                           quote="\"", comment="", strip.white=T)[,c("lni", "date", "legaltopics")]
           nt <- apply(as.matrix(z[,"legaltopics"]), 1, function(a) length(strsplit(a, "\\|")[[1]]))
           data.frame("file"=f, z[,c("lni", "date", "legaltopics")], "ntopics"=nt)
         }))

# Retrieve counts of legal topics by case in the December data
# Note that topics may be repeated by case (many instances have been observed)
# Each duplication is counted here
y <- do.call(rbind,
       lapply(0:11,
         function(i) {
           f <- sprintf("%s%02.0f", "DukePart", i)
           z <- read.table(gzfile(paste(lnsourcedir2, "\\", f, ".csv.gz", sep="")), header=T, sep=",",
                           quote="\"", comment="", strip.white=T)[,c("lni", "date", "legaltopicpguids")]
           nt <- apply(as.matrix(z[,"legaltopicpguids"]), 1, function(a) length(strsplit(a, "\\|")[[1]]))
           data.frame("file"=f, z[,c("lni", "date", "legaltopicpguids")], "ntopics"=nt)
         }))

# Merge topic counts by case
# Retain cases missing in one data set or the other
z <- merge(x, y, by="lni", all.x=T, all.y=T)
names(z) <- c("lni", "file1", "date1", "ID1", "ntopics1", "file2", "date2", "ID2", "ntopics2")

# Convert counts to 0 for cases missing in one set or the other
head(z)
z[which(is.na(z[,"ntopics1"])),"ntopics1"] <- 0
z[which(is.na(z[,"ntopics2"])),"ntopics2"] <- 0

# Save a table of topic counts by data set and case
write.table(z[which(z[,"ntopics1"]!=z[,"ntopics2"]),], "LNI-Source-File-n-Topics.csv", row.names=F, col.names=T, quote=F, sep=",")

# Review cases where topic counts are different between data sets and count is zero in December
z[which(z[,"ntopics1"]!=z[,"ntopics2"] & z[,"ntopics2"]==0),c("lni", "ntopics1", "ntopics2")]

# Review cases where topic counts are different between data sets and count is non-zero in December
z[which(z[,"ntopics1"]!=z[,"ntopics2"] & z[,"ntopics2"]>0),]

# Enumerate cases with differing numbers of cases by year
aggregate(1:nrow(z), by=list(substring(z[,"date1"], 1, 4)), function(k) mean(z[k,"ntopics1"]-z[k,"ntopics2"]))

# Review specific cases
lni <- c("460R-YCG0-0038-X4HM-00000-00", "3S4X-00B0-003B-G2TV-00000-00", "3S4W-Y8X0-0039-M0SN-00000-00", "3S4X-0V20-008H-V19Y-00000-00")[4]
lni <- "5H1H-YR91-F04K-V00T-00000-00"
lni <- "5H1J-2SN1-F04K-R004-00000-00"
x[which(x[,"lni"] %in% lni),]
y[which(y[,"lni"] %in% lni),]


#######################################################################################
#### Sample cases with legal topics in March, but none in December
#######################################################################################

x <- dbGetQuery(db1, "select a.LNI, c.ShortName, a.CaseTitleShort, a.DecisionDate
                      from   CaseHeader a join Appeals2.CaseHeader b on a.LNI=b.LNI
                             join Court c on a.CourtID=c.ID
                      where  a.LNI in(select LNI from CaseLegalTopics)
                             and a.LNI not in(select LNI from Appeals2.CaseLegalTopics)")
colnames(x) <- c("LNI","Court","CaseTitle","DecisionDate")

write.table(x[sample(1:nrow(x), 100, replace=F),], "LNI-With-March-Without-December-Topics.csv", row.names=F, col.names=T, sep=",")


#######################################################################################
#### Identify cases with at least one (identical) legal topic appearing in both March and December
#######################################################################################

x <- dbGetQuery(db1, "select distinct a.LNI, a.CaseTitleShort, a.DecisionDate
                      from   CaseHeader a join Appeals.CaseLegalTopics b on a.LNI=b.LNI
                             join Appeals2.CaseLegalTopics c on a.LNI=c.LNI and b.TopicID=c.TopicID")

nrow(x)

