# Duke University Law Appeals Analysis
# Review of 2019-12-03 LexisNexis Data
# Citations

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

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-03-28")
lnsourcedir1 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-03-13"
lnsourcedir2 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03"
imgdir <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-03-28"


#######################################################################################
#### Connect to Appeals databases
#### db1 for data set 1, March 2019
#### db2 for data set 2, December 2019
#### Note that a single citation file was supplied by LN
#### It was imported into data sets 1 and 2
#######################################################################################

usr <- "tjb48"
#dbDisconnect(db1)
#dbDisconnect(db2)
db1 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals", user=usr, password=rstudioapi::askForPassword("Password:  "))
db2 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals2", user=usr, password=rstudioapi::askForPassword("Password:  "))


#######################################################################################
#### List table structures
#######################################################################################

dbGetQuery(db1, "show tables")
dbGetQuery(db2, "show tables")
dbGetQuery(db1, "describe CaseHeader")
dbGetQuery(db2, "describe CaseHeader")
dbGetQuery(db2, "describe CaseHeaderExt")
dbGetQuery(db1, "describe Court")
dbGetQuery(db2, "describe Court")
dbGetQuery(db1, "describe Citation")
dbGetQuery(db2, "describe Citation")
dbGetQuery(db1, "describe ShepardTreatment")


#######################################################################################
# Citation file structure
#######################################################################################

# From SQL DB creation script:
# Double quote third virtual column (NormCite)
# Note that 2019-03-13 source IndexLNI file does not contain column headers
# Some NormCite entries contain double quotes, so replace existing ones with !

# f0 <- unz("LNItoLNItreatments.zip", "LNItoLNItreatments.csv")
# f1 <- gzfile(f1<-"Citation.csv.gz")
# x <- scan(f0, what="character", sep="\n", quote="", comment.char="", strip.white=T)
# Inspect row 1 (where column IDs would be expected)
# x[1]
# Substitute double quotes
# x <- gsub("\"", "!", x)
# # identify locations of commas that delimit third column (second and last commas)
# p <- gregexpr(",", x)
# # Report comma frequency
# n <- unlist(lapply(p, length))
# table(n)
# # Write modified records
# write(c("LNI, LNICited, NormCitation, ShepardTreatment\r",
        # apply(as.matrix(1:length(x)), 1,
              # function(i) {
                # if(i%%10000==0) cat(i, " ")
                # paste(substring(x[[i]], 1, p[[i]][2]), "\"",
                      # substring(x[[i]], p[[i]][2]+1, p[[i]][3]-1), "\"",
                      # substring(x[[i]], p[[i]][length(p[[i]])], nchar(x[[i]])), "\r", sep="")
              # })), f1, sep="\n")
# close(f0)
# close(f1)

# End of SQL DB creation script excerpt

# Attempt to read source citations file as a csv table
x <- read.table(unz(paste(lnsourcedir1, "\\LNItoLNItreatments.zip", sep=""), "LNItoLNItreatments.csv"), header=F, sep=",")

# Review header and malformed csv records
y <- scan(unz(paste(lnsourcedir1, "\\LNItoLNItreatments.zip", sep=""), "LNItoLNItreatments.csv"), what="character", sep="\n")
y[1]
z <- apply(as.matrix(y), 1, function(a) length(strsplit(a, ",", fixed=T)[[1]]))
table(z)
y[which(z!=4)]

# Latex table of sample of citation recs 
a <- ""
for(i in sample(1:length(y), 20, replace=F))
  a <- c(a, paste(gsub(",", " & ", y[i]), "\\\\", sep=""))
writeLines(a)

# Latex table of invalid citation recs
w <- apply(as.matrix(y[sample(which(z==5), 20, replace=F)]), 1,
       function(y) {
         a <- strsplit(y, ",", fixed=T)[[1]]
         paste(a[1], " & ", a[2], " & ", a[3], ",", a[4], " & ", a[5], sep="")
       })
a <- ""
for(i in 1:length(w))
  a <- c(a, paste(w[i], "\\\\", sep=""), "& & &\\\\[-6pt]")
writeLines(a)

# Records containing ","
y[grep("\", \"", y)]


#######################################################################################
#### Evaluate citation LNI format
#######################################################################################

# LNIs have format XXXX-XXXX-XXXX-XXXX-00000-0X, where each X is either a
# single integer (0-9) or upper case character (A-Z)

# Select data set
db <- db2

# Identify LNIs that do not conform to the official pattern
# The following grep pattern is taken from a review of the March data
x <- dbGetQuery(db,
       paste(" select LNI",
             " from   Citation",
             " where  not LNI regexp '[34578][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                                     "[0123456789BCDFGHJKMNPRSTVWXY]{3}[01]-",
                                     "[026DFJKTY][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                                     "[0123456789BCDFGHJKMNPRSTVWXY][0-5][0123456789BCDFGHJKMNPRSTVWXY]{2}-",
                                     "00000-0[0-5]'", sep=""))[,"LNI"]

# Enumerate non-conforming LNIs
length(x)

# Cited LNIs
x <- dbGetQuery(db,
       paste(" select LNICited",
             " from   Citation",
             " where  not LNICited regexp '[34578][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                                          "[0123456789BCDFGHJKMNPRSTVWXY]{3}[012]-",
                                          "[026DFJKNRTY][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                                          "[0123456789BCDFGHJKMNPRSTVWXY][0-5][0123456789BCDFGHJKMNPRSTVWXY]{2}-",
                                          "00000-0[0-5]'", sep=""))[,"LNICited"]
length(x)

# List, by each position the set of characters appearing in that position
lapply(1:28, function(i) paste(sort(unique(substring(x, i, i))), collapse=""))

# Sample non-conforming LNIs
x[1:100]


#######################################################################################
#### Enumerate citations
#######################################################################################

db <- db1

# Total
dbGetQuery(db, "select count(1) from Citation")

# Distinct LNIs
dbGetQuery(db, "select count(distinct lni) from Citation")
dbGetQuery(db, "select count(distinct lnicited) from Citation")

# LNIs appearing in case headers
dbGetQuery(db, "select count(distinct lni) from Citation where lni in(select lni from CaseHeader)")
dbGetQuery(db, "select count(distinct lnicited) from Citation where lnicited in(select lni from CaseHeader)")


#######################################################################################
#### Tabulate number of citations per citing case
#######################################################################################

# Append data frames of citation counts by case, one from each data set
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
                        "select   case when(b.n is not null)then b.n else 0 end as nCitation, count(1) as nCase
                         from     CaseHeader a
                                  left join(select   LNI, count(1) as n
                                            from     Citation
                                            group by LNI) b on a.LNI=b.LNI
                         group by b.n"))
         }))

# Generate plot with one symbol per data set
png(paste(imgdir, "\\Citations\\images\\DistributionNumberOfCitationsPerCase.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=x, aes(x=nCitation, y=log(nCase)/log(10), shape=set)) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(labels=function(x) format(x, big.mark=",")) +
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
  labs(x="\nnumber of citations per citing case", y="number of cases\n")
dev.off()


#######################################################################################
#### Tabulate number of citations per cited case
#######################################################################################

# Append data frames of citation counts by case, one from each data set
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
                        "select   case when(b.n is not null)then b.n else 0 end as nCitation, count(1) as nCase
                         from     CaseHeader a
                                  left join(select   LNICited, count(1) as n
                                            from     Citation
                                            group by LNICited) b on a.LNI=b.LNICited
                         group by b.n"))
         }))

# Reorder data sets for legend appearance
x[,"set"] <- factor(x[,"set"], levels=c("A", "B"))

# Generate plot with one symbol per data set
png(paste(imgdir, "\\Citations\\images\\DistributionNumberOfCitationsPerCitedCaseB.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=x, aes(x=nCitation, y=log(nCase)/log(10), shape=set)) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(limits=c(0, 1500), labels=function(x) format(x, big.mark=",")) +
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
  labs(x="\nnumber of citations per cited case", y="number of cases\n")
dev.off()

dbGetQuery(db1, "select *
                 from   CaseHeader a
                        join (select   lnicited, count(1) as n
                              from     Citation
                              group by lnicited
                              having   count(1)>1500) b on a.lni=b.lnicited")

dbGetQuery(db1, "select * from Court")


#######################################################################################
#### Tabulate number of citations by year
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
                         from     CaseHeader a join Citation b on a.LNI=b.LNI
                         group by year(a.DecisionDate)"))
         }))

# Reorder data sets so that March appears first in legends
x[,"set"] <- factor(x[,"set"], levels=c("A", "B"))

# Review min and max years by data set
dbGetQuery(db1, "select min(year(DecisionDate)), max(year(DecisionDate)) from CaseHeader")
dbGetQuery(db2, "select min(year(DecisionDate)), max(year(DecisionDate)) from CaseHeader")

png(paste(imgdir, "\\Citations\\images\\DistributionNumberOfCitationsByYear.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  # Overlayed area plot
  # Position="identity" aesthetics causes overlay instead of the default stacking
  #geom_area(data=x, aes(x=yr, y=n, fill=set, group=set), alpha=0.35, position="identity") +
  #scale_fill_manual(values=c("A"="red", "B"="blue")) +
  geom_point(data=x, aes(x=yr, y=n, shape=set)) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
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
  labs(x="\nyear", y="number of citations\n")
dev.off()


#######################################################################################
#### Tabulate number of citations by court and year
#######################################################################################

# Append data frames of case counts by court and year, one from each data set
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
                        "select   c.ShortName as court, year(a.DecisionDate) as yr, count(1) as n
                         from     CaseHeader a join Citation b on a.LNI=b.LNI
                                  join Court c on a.CourtID=c.ID
                         group by c.ShortName, year(a.DecisionDate)"))
         }))

# Abbreviate court names
unique(x[,"court"])
x[,"court"] <- sub("Circuit Court of Appeals", "Circ", x[,"court"])
x[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", x[,"court"])
x[,"court"] <- sub("Court of Federal Claims", "Fed Claims", x[,"court"])
x[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", x[,"court"])
x[,"court"] <- sub("Judicial Conference, Committee on Judicial Conduct", "Jud Conduct", x[,"court"])
x[,"court"] <- sub("Temporary Emergency Court of Appeals", "Temp Emergency", x[,"court"])
x[,"court"] <- sub("Tennessee Eastern District Court", "Tenn E Dist", x[,"court"])
x[,"court"] <- sub("Texas Southern District Court", "Tex S Dist", x[,"court"])
sort(unique(x[,"court"]))
x[,"court"] <- factor(x[,"court"], levels=c("", "1st Circ", "2nd Circ", "3rd Circ", "4th Circ", "5th Circ",
                                            "6th Circ", "6th Circ Bkruptcy", "7th Circ", "8th Circ",
                                            "9th Circ", "10th Circ", "11th Circ", "DC Circ", "Fed Claims",
                                            "Federal Circ", "Jud Conduct", "Temp Emergency", "Tenn E Dist",
                                            "Tex S Dist"))   

# Review min and max years by data set
dbGetQuery(db1, "select min(year(DecisionDate)), max(year(DecisionDate)) from CaseHeader")
dbGetQuery(db2, "select min(year(DecisionDate)), max(year(DecisionDate)) from CaseHeader")

png(paste(imgdir, "\\Citations\\images\\DistributionNumberOfCitationsByCourtYear.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  # Overlayed area plot
  # Position="identity" aesthetics causes overlay instead of the default stacking
  #geom_area(data=x, aes(x=yr, y=n, fill=set, group=set), alpha=0.35, position="identity") +
  #scale_fill_manual(values=c("A"="red", "B"="blue")) +
  geom_point(data=x, aes(x=yr, y=n, shape=set)) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(breaks=seq(1974, 2018, 2)) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  facet_wrap(~court) +
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
  labs(x="\nyear", y="number of citations\n")
dev.off()


#######################################################################################
#### Review cases with no citations
#######################################################################################

db <- db2

dbGetQuery(db,
  "select count(1) as n,
          sum(case when(LNI not in(select distinct LNI from Citation))then 1 else 0 end) as n0
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
#### Identify LNIs or cited LNIs that do not appear in supplied cases
#######################################################################################

# Select data set
db <- db2

# Citing LNIs that do not appear on cases
x <- dbGetQuery(db, "select distinct LNI from Citation where LNI not in(select LNI from CaseHeader)")[,"LNI"]
length(x)

# Citations for existing cases, but where cited LNI does not appear on a case 
x <- dbGetQuery(db,
       "select distinct b.LNICited
        from   CaseHeader a join Citation b on a.LNI=b.LNI
        where  b.LNICited not in(select LNI from CaseHeader)")[,"LNICited"]
length(x)


#######################################################################################
#### Evaluate Shepard's treatment existence
#### Note that no citations were supplied with the December data
#### Citations uploaded into the Appeals2 database are identical to those in the
#### Appeals database
#######################################################################################

# Select data set
db <- db2

dbGetQuery(db, "select * from ShepardTreatment limit 10")
dbGetQuery(db, "select * from Citation limit 10")

# Retrieve unknown treatments
x <- dbGetQuery(db, "select *
                     from   Citation
                     where  ShepardTreatmentID not in(select ID from ShepardTreatment)")
nrow(x)


#######################################################################################
#### Distribution of the difference in number of cases referencing Shepard treatments
#######################################################################################

# Retrieve March and December frequenciew by letter
x <- dbGetQuery(db1,
       "select   c.Letter, count(1)
        from     CaseHeader a join Citation b on a.LNI=b.LNI
                 join ShepardTreatment c on b.ShepardTreatmentID=c.ID
        group by c.Letter")
nrow(x)
y <- dbGetQuery(db2,
       "select   c.Letter, count(1)
        from     CaseHeader a join Citation b on a.LNI=b.LNI
                 join ShepardTreatment c on b.ShepardTreatmentID=c.ID
        group by c.Letter")
nrow(y)

# Merge March and December counts by treatment ID
# Retain topics that do not appear in the alternate dat set
z <- merge(x, y, by="Letter", all.x=T, all.y=T)
colnames(z) <- c("ShepardLetter", "n1", "n2")

# Convert counts to 0 for topics missing in one data set
z[1:100,]
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0

# Compute the difference in counts, between data sets, by letter
z[,"nDiff"] <- z[,"n2"]-z[,"n1"]

# Inspect maximum frequencies
max(z[,"n1"])
max(z[,"n2"])

# List letters with maximum frequency
z[which(z[,"n1"]==max(z[,"n1"])),]
z[which(z[,"n2"]==max(z[,"n2"])),]

# Plot bars of the difference in counts, between sets, by letter
png(paste(imgdir, "\\Citations\\images\\DifferenceInCasesByShepardLetter.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_bar(data=z, aes(x=ShepardLetter, y=ifelse(nDiff>0, log(nDiff), 0)), stat="identity", color="gray85", fill="blue3") +
  coord_flip() +
  #scale_x_continuous(limits=c(-2500, 500), labels=function(x) format(x, big.mark=",")) +
  scale_y_continuous(breaks=log(c(1, 10, 100, 1000, 10000, 100000)),
                     labels=format(c(1, 10, 100, 1000, 10000, 100000), big.mark=",")) +
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
  labs(x="Shepard letter\n", y="\ndifference in number of citations, set B - set A")
dev.off()


#######################################################################################
#### Distribution of the difference in number of cases by year for Shepard treatment
#### letters &f, c, ca, d, e, f (those with case differences > 1000 between datasets)
#######################################################################################

# Retrieve March and December frequencies by year and letter
x <- dbGetQuery(db1,
       "select   year(a.DecisionDate) as year, c.Letter, count(1)
        from     CaseHeader a join Citation b on a.LNI=b.LNI
                 join ShepardTreatment c on b.ShepardTreatmentID=c.ID
        where    c.Letter in('&f', 'c', 'ca', 'd', 'e', 'f')
        group by year(a.DecisionDate), c.Letter")
nrow(x)
y <- dbGetQuery(db2,
       "select   year(a.DecisionDate) as year, c.Letter, count(1)
        from     CaseHeader a join Citation b on a.LNI=b.LNI
                 join ShepardTreatment c on b.ShepardTreatmentID=c.ID
        where    c.Letter in('&f', 'c', 'ca', 'd', 'e', 'f')
        group by year(a.DecisionDate), c.Letter")
nrow(y)

# Merge March and December counts by treatment ID
# Retain topics that do not appear in the alternate dat set
z <- merge(x, y, by=c("year", "Letter"), all.x=T, all.y=T)
colnames(z) <- c("year", "ShepardLetter", "n1", "n2")

# Convert counts to 0 for topics missing in one data set
z[1:100,]
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0

# Compute the difference in counts, between data sets, by letter
z[,"nDiff"] <- z[,"n2"]-z[,"n1"]

min(z[,"year"])
max(z[,"year"])

png(paste(imgdir, "\\Citations\\images\\DifferenceInCasesShepardLetterByYear.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=z[which(z[,"nDiff"]>=0),], aes(x=year, y=ifelse(nDiff>0, log(nDiff), 0)), color="black", alpha=0.5) +
  geom_point(data=z[which(z[,"nDiff"]<0),], aes(x=year, y=log(-nDiff)), color="red", alpha=0.5) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  scale_y_continuous(breaks=log(c(1, 10, 100, 1000, 10000, 100000)),
                     labels=format(c(1, 10, 100, 1000, 10000, 100000), big.mark=",")) +
  facet_wrap(~ShepardLetter) +
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
        axis.text.x=element_text(size=10, angle=90, hjust=0, vjust=0.5),
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
  labs(x="\nyear", y="difference in number of citations\n")
dev.off()

#######################################################################################
#### Evaluate duplicated citations (multiple reporters)
#######################################################################################

dbGetQuery(db1, "select * from Citation limit 20")
dbGetQuery(db1, "select distinct PrimaryReporter from Citation")

x <- dbGetQuery(db1, "select   a.LNI, a.LNICited, a.NormCitation, c.Letter
                      from     Citation a join (select   LNI, LNICited
                                                from     Citation
                                                group by LNI, LNICited
                                                having   count(1)=5
                                                limit    10) b on a.lni=b.lni and a.lnicited=b.lnicited
                               join ShepardTreatment c on a.ShepardTreatmentID=c.ID
                      order by a.LNI, a.LNICited")


dbGetQuery(db1, "select * from Citation where normcite like '%wl%')
