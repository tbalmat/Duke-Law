# Duke University Law Appeals Analysis
# Review of 2019-12-03 LexisNexis Data

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(ggplot2)
library(xtable)
library(DBI)
library(RMySQL)

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-02-21")
lnsourcedir1 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-03-13"
lnsourcedir2 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03"
imgdir <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-02-21"


#######################################################################################
#### Connect to Appeals database
#######################################################################################

# Connect to Appeals database
db1 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals", user="tjb48", password=rstudioapi::askForPassword("Password:  "))
db2 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals2", user="tjb48", password=rstudioapi::askForPassword("Password:  "))

#######################################################################################
#### List tables
#######################################################################################

dbGetQuery(db1, "show tables")
dbGetQuery(db2, "show tables")
dbGetQuery(db1, "describe CaseHeader")
dbGetQuery(db2, "describe CaseHeader")
dbGetQuery(db1, "describe Court")
dbGetQuery(db2, "describe Court")

#######################################################################################
#### Tabulate cases by year 
#######################################################################################

x <- dbGetQuery(db2, "select year(DecisionDate), count(1) as n from CaseHeader group by year(DecisionDate)")
sum(x[,"n"])

# Verify that empty dates exist in source files
mdate <- t(apply(as.matrix(0:10), 1,
             function(i) {
               x <- read.table(gzfile(paste("dukePart", sprintf("%02d", i), ".csv.gz", sep="")),
                               header=T, sep=",", quote="\"", comment.char="", strip.white=T, as.is=T)[,"date"]
               # class(x)
               table(nchar(gsub(" ", "", x)))
             }))
sum(mdate[,1])

#######################################################################################
#### Test for duplicate LNI in case headers 
#######################################################################################

dbGetQuery(db1, "select LNI, count(1) as n from CaseHeader group by LNI having count(1)>1")
dbGetQuery(db2, "select LNI, count(1) as n from CaseHeader group by LNI having count(1)>1")

#######################################################################################
#### Tabulate cases by type 
#######################################################################################

# Test for duplicate case, type combinations
dbGetQuery(db1, "select LNI, CaseType, count(1) as n from CaseType group by LNI, CaseType having count(1)>1") 
dbGetQuery(db2, "select LNI, CaseType, count(1) as n from CaseType group by LNI, CaseType having count(1)>1") 

# Enumerate types
dbGetQuery(db1, "select CaseType, count(1) from CaseType group by CaseType") 
dbGetQuery(db2, "select CaseType, count(1) from CaseType group by CaseType") 

# Aggregate cases by year and type
# R warnings may be generated here
# They can be suppressed with suppressWarnings(), but then all warnings are concealed
x <- dbGetQuery(db2,
       "select   year(DecisionDate) as year, count(1) as n,
                 sum(case when(b.nCriminal=0 and nCivil>0)then 1 else 0 end) as nCivil,
                 sum(case when(b.nCriminal>0 and nCivil=0)then 1 else 0 end) as nCriminal,
                 sum(case when(b.nCriminal>0 and b.nCivil>0)then 1 else 0 end) as nCrimCiv,
                 sum(case when(b.LNI is null)then 1 else 0 end) as nNone
        from     CaseHeader a
                 left join (select   LNI,
                                     sum(case when(CaseType='Criminal')then 1 else 0 end) as nCriminal,
                                     sum(case when(CaseType='Civil')then 1 else 0 end) as nCivil
                            from     CaseType
                            group by LNI) b on a.LNI=b.LNI
        group by year(DecisionDate)")

ltx <- apply(as.matrix(x), 1, function(x) paste(paste(format(x, big.mark=","), collapse=" & ", sep=""), "\\\\", sep=""))
writeLines(ltx)

# Sum cases by type
y <- apply(as.matrix(c("n", "nCivil", "nCriminal", "nCrimCiv", "nNone")), 1, function(a) sum(x[,a]))
sum(y[2:5])

y <- data.frame("year"=x[,"year"],
                "type"=c(rep("criminal", nrow(x)), rep("civil", nrow(x)), rep("crim-civ", nrow(x)), rep("none", nrow(x))),
                "n"=c(x[,"nCriminal"], x[,"nCivil"], x[,"nCrimCiv"], x[,"nNone"]))

#dev.new()
#png(paste(imgdir, "\\YearCaseTypeDensity.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_area(data=y, aes(x=year, y=n, fill=type), position="stack", alpha=0.75) +
  scale_fill_manual(name="", values=c("none"="blue2", "criminal"="orange2", "civil"="yellow3", "crim-civ"="purple2")) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  scale_y_continuous(label=function(x) format(x, big.mark=",")) +
  labs(x="\nyear", y="cases\n") +
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
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="", y="cases\n")
#dev.off()

#######################################################################################
#### Plot case frequency by type and year, paneled by source file
#######################################################################################

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03")

z <- data.frame()
for(i in 0:10) {
  x <- read.table(gzfile(paste("dukePart", sprintf("%02d", i), ".csv.gz", sep="")),
                  header=T, sep=",", quote="\"", comment.char="", strip.white=T, as.is=T)
  y <- t(as.matrix(table(x[,"casetypes"], substring(x[,"date"], 1, 4))))
  y <- y[which(nchar(rownames(y))>0),]
  z <- rbind(z, data.frame("i"=i, "year"=rep(as.numeric(rownames(y)), 4),
                           "type"=c(rep("none", nrow(y)), rep("civil", nrow(y)), rep("civil-crim", nrow(y)), rep("crim", nrow(y))),
                           "n"=as.vector(y)))
}

png(paste(imgdir, "\\DistributionCaseTypeInSourceFile.png", sep=""), res=300, height=2400, width=2400)
ggplot() +
  geom_area(data=z, aes(x=year, y=n, fill=type), position="stack", alpha=0.85) +
  scale_fill_manual(name="", values=c("none"="blue2", "crim"="orange2", "civil"="yellow3", "civil-crim"="purple2")) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  scale_y_continuous(label=function(x) format(x, big.mark=",")) +
  facet_wrap(~i, labeller=as_labeller(function(i) paste("file ", i, sep=""))) +
  labs(x="\nyear", y="cases\n") +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing=unit(0, "in"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=10, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=10),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
        labs(title="Duke Appeals Project - LexisNexis Data",
             subtitle="Distribution of Case Type within Source File\n",
             x="\nyear", y="cases\n")
dev.off()

#######################################################################################
#### Tabulate cases by court (note that court must not be null in case header)
#######################################################################################

x <- dbGetQuery(db,
       "select   year(a.DecisionDate) as Year, b.ShortName, b.LongName, count(1) as n
        from     CaseHeader a left join Court b on a.CourtID=b.ID
        group by year(a.DecisionDate), b.ShortName, b.LongName")

x[,"court"] <- factor(x[,"ShortName"],
                      levels=c("1st Circuit Court of Appeals",
                               "2nd Circuit Court of Appeals",
                               "3rd Circuit Court of Appeals",
                               "4th Circuit Court of Appeals",
                               "5th Circuit Court of Appeals",
                               "6th Circuit Court of Appeals",
                               "6th Circuit Bankruptcy Appellate Panel",
                               "7th Circuit Court of Appeals",
                               "8th Circuit Court of Appeals",
                               "9th Circuit Court of Appeals",
                               "10th Circuit Court of Appeals",
                               "11th Circuit Court of Appeals",
                               "Court of Federal Claims",
                               "DC Circuit Court of Appeals",
                               "Federal Circuit Court of Appeals",
                               "Temporary Emergency Court of Appeals",
                               "Tennessee Eastern District Court"))

# Histogram, distribution by court
png(paste(imgdir, "\\CourtCaseDistribution.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_bar(data=setNames(aggregate(x[,"n"], by=list(x[,"court"]), sum), c("court", "n")),
           aes(x=court, y=n), stat="identity", fill="blue3") +
  scale_y_continuous(label=function(x) format(x, big.mark=",")) +
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
  labs(x="", y="cases\n")
dev.off()

# Line graph, cases by court and year
png(paste(imgdir, "\\CourtYearCaseLineGraph.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_line(data=x, aes(x=Year, y=n)) +
  scale_x_continuous(breaks=seq(1974, 2018, 6)) +
  scale_y_continuous(label=function(x) format(x, big.mark=",")) +
  facet_wrap(~court, labeller=as_labeller(function(x) sub("rict Court", "",
                                                      sub("Court of ", "",
                                                      sub(" Appellate Panel", "",
                                                      sub(" Court of Appeals", "", x)))))) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing=unit(0, "in"),
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
  labs(x="", y="cases\n")
dev.off()

#######################################################################################
#### Identify cases with missing short or long court name 
#######################################################################################

x <- dbGetQuery(db2,
                "select   b.ShortName, count(1)
                 from     CaseHeader a left join Court b on a.CourtID=b.ID
                 group by b.ShortName")

dbGetQuery(db2, "select * from CaseHeader where CourtID not in(select ID from Court)")
dbGetQuery(db2, "select * from Court")

#######################################################################################
#### Identify cases with missing panel 
#######################################################################################

x <- dbGetQuery(db, "select a.LNI from CaseHeader a left join Panel b on a.LNI=b.LNI where b.LNI is null")
dbGetQuery(db, "select count(1) from CaseHeader a left join Panel b on a.LNI=b.LNI where b.LNI is null")

#######################################################################################
#### Tabulate panel member count 
#######################################################################################

x <- dbGetQuery(db,
       "select   b.ShortName,
                 case when(c.njudge is not null)then c.njudge else 0 end as njudge, count(1) as ncase
        from     CaseHeader a
                 left join Court b on a.CourtID=b.ID
                 left join (select LNI, count(1) as njudge from Panel group by LNI) c on a.LNI=c.LNI
        group by b.ShortName, case when(c.njudge is not null)then c.njudge else 0 end")

x[,"court"] <- factor(x[,"ShortName"],
                      levels=c("1st Circuit Court of Appeals",
                               "2nd Circuit Court of Appeals",
                               "3rd Circuit Court of Appeals",
                               "4th Circuit Court of Appeals",
                               "5th Circuit Court of Appeals",
                               "6th Circuit Court of Appeals",
                               "6th Circuit Bankruptcy Appellate Panel",
                               "7th Circuit Court of Appeals",
                               "8th Circuit Court of Appeals",
                               "9th Circuit Court of Appeals",
                               "10th Circuit Court of Appeals",
                               "11th Circuit Court of Appeals",
                               "Court of Federal Claims",
                               "DC Circuit Court of Appeals",
                               "Federal Circuit Court of Appeals",
                               "Temporary Emergency Court of Appeals",
                               "Tennessee Eastern District Court"))

png(paste(imgdir, "\\PanelSizeDistribution.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_vline(data=data.frame("x"=seq(0, 22, 2)), aes(xintercept=x), color="gray85") +
  geom_line(data=setNames(aggregate(x[,"ncase"], by=list(x[,"njudge"]), sum), c("njudge", "ncase")),
            aes(x=njudge, y=ncase)) +
  scale_x_continuous(breaks=0:22) +
  scale_y_continuous(label=function(x) format(x, big.mark=",")) +
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
  labs(x="\nnumber of judges in panel", y="number of cases\n")
dev.off()

png(paste(imgdir, "\\PanelSizeCourtLineGraph.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_vline(data=data.frame("x"=seq(0, 22, 2)), aes(xintercept=x), color="gray85") +
  geom_line(data=x, aes(x=njudge, y=ncase)) +
  scale_x_continuous(breaks=seq(0, 22, 4)) +
  scale_y_continuous(label=function(x) format(x, big.mark=",")) +
  facet_wrap(~court, labeller=as_labeller(function(x) sub("rict Court", "",
                                                      sub("Court of ", "",
                                                      sub(" Appellate Panel", "",
                                                      sub(" Court of Appeals", "", x)))))) +

  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing=unit(0, "in"),
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
  labs(x="\nnumber of judges in panel", y="number of cases\n")
dev.off()

# Verify that some cases have no panel using source files
mpanel <- apply(as.matrix(0:10), 1,
            function(i) {
              x <- read.table(gzfile(paste("dukePart", sprintf("%02d", i), ".csv.gz", sep="")),
                              header=T, sep=",", quote="\"", comment.char="", strip.white=T, as.is=T)[,"judgeentitylist"]
              length(which(nchar(gsub(" ", "", x))==0))
           })
#head(x[which(nchar(gsub(" ", "", x[,"judgeentitylist"]))==0),"lni"])

# Inspect panel in source files with many members
x <- read.table(gzfile(paste("dukePart", sprintf("%02d", 0), ".csv.gz", sep="")),
                              header=T, sep=",", quote="\"", comment.char="", strip.white=T, as.is=T)[,"judgeentitylist"]
x[which(nchar(x)>14*20)]

#######################################################################################
#### Identify judge IDs that do not appear in the Judges table
#######################################################################################

dbGetQuery(db, "select distinct JudgeID from Panel where JudgeID not in(select ID from Judge)")

#######################################################################################
#### Tabulate cases by year and pub stat
#######################################################################################

x <- dbGetQuery(db,
       "select   year(DecisionDate) as year, PubStatus, count(1) as n
        from     CaseHeader
        group by year(DecisionDate), PubStatus")

x[which(x[,"PubStatus"]==""), "PubStatus"] <- "none"
x[,"PubStatus"] <- factor(x[,"PubStatus"], levels=c("Reported", "Unreported", "none"))

png(paste(imgdir, "\\YearPubStatDistribution.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_area(data=x, aes(x=year, y=n, fill=PubStatus, group=PubStatus), position="identity", alpha=0.5) +
  scale_fill_manual(name="", values=c("Reported"="green3", "Unreported"="blue3", "none"="Red3")) +
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
  labs(x="", y="number of cases\n")
dev.off()

#######################################################################################
#### List cases with no citations
#######################################################################################

dbGetQuery(db, "select count(1)
                from   CaseHeader a left join Citation b on a.LNI=b.LNI
                       join Citation c on a.LNI=c.LNICited
                where  b.lni is null or c.LNI is null")

#######################################################################################
#### Tabulate number of citations
#######################################################################################

x <- dbGetQuery(db,
       "select   case when(b.n is not null)then b.n else 0 end as nCitation, count(1) as nCase
        from     CaseHeader a
                 left join(select   LNI, count(1) as n
                           from     Citation
                           group by LNI) b on a.LNI=b.LNI
        group by b.n")

png(paste(imgdir, "\\CaseCitationDistribution.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_line(data=x, aes(x=nCitation, y=log(nCase)/log(10))) +
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
  labs(x="\nnumber of citations", y="number of cases (log-10)\n")
dev.off()


#######################################################################################
#### Compute proportion of per curiam cases by year
#######################################################################################

x <- dbGetQuery(db,
       "select   year(DecisionDate) as year,
                 sum(convert(PerCuriam, unsigned))*1./count(1) as p
        from     CaseHeader
        group by year(DecisionDate)")

png(paste(imgdir, "\\PerCuriamCasesByYear.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_line(data=x, aes(x=year, y=p)) +
  #scale_y_continuous(breaks=1:6, labels=format(10**(1:6), big.mark=",")) +
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
  labs(x="\nyear", y="proportion per curiam cases\n")
dev.off()

#######################################################################################
#### Logistic regression model for P(per curiam | year, case type, and court)
#######################################################################################

dbGetQuery(db, "show tables")
dbGetQuery(db, "describe CaseHeader")
dbGetQuery(db, "describe CaseTypeComposite")
dbGetQuery(db, "select LNI from CaseHeader where LNI not in(select LNI from CaseTypeComposite)")

x <- dbGetQuery(db,
       "select   year(ch.DecisionDate) as year, ch.CourtID, ct.CaseType,
                 count(1) as n,
                 sum(convert(ch.PerCuriam, unsigned))*1./count(1) as p
        from     CaseHeader ch join CaseTypeComposite ct on ch.LNI=ct.LNI
        where    ch.DecisionDate>0
        group by year(ch.DecisionDate), ch.CourtID, ct.CaseType")
x[,"year"] <- factor(x[,"year"])
x[,"CourtID"] <- factor(x[,"CourtID"])
x[,"CaseType"] <- factor(x[,"CaseType"])

m <- glm(p~year+CourtID+CaseType, family=binomial, data=x, weights=x[,"n"])
m <- glm(p~year+CourtID+CaseType, family=quasibinomial, data=x, weights=x[,"n"])
xtable(summary(m))


#######################################################################################
#### Evaluate duplication of opinions where one is missing panel, others not
#### This is accomplished by joining two separate queries here, since MySQL stalls
#### while attempting to execute a single join query
#######################################################################################

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review")

# Retrieve cases with no panel
casea <- dbGetQuery(db,
           " select lni, courtid, decisiondate, casetitlelexisnexis
             from   CaseHeader
             where  lni not in(select lni from Panel)")

# Retrieve cases with panel
caseb <- dbGetQuery(db,
           " select ch.lni, ch.courtid, ch.decisiondate, ch.casetitlelexisnexis, pn.nj
             from   CaseHeader ch
                    join (select lni, count(1) as nj from Panel group by lni) pn on ch.lni=pn.lni")

# Normalize titles
normttl <- function(t) {
          # Convert to lower case
          t <- tolower(t)
          # Omit reporter (omit final comma and text following it)
          p <- gregexpr(',', t)[[1]]
          if(p[1]>0)
            t <- substring(t, 1, p[length(p)]-1)
          # Standardize text
          t <- gsub("u\\.s\\.", "united states", t)
          t <- gsub("usa", "united states", t)
          t <- gsub("us", "united states", t)
          t <- gsub("div\\.", "division", t)
          t <- gsub("co\\.", "company", t)
          t <- gsub("corp\\.", "corporation", t)
          t <- gsub("const\\.", "construction", t)
          t <- gsub("\\&", "and", t)
          t <- gsub("ass'n", "association", t)
          t <- gsub("asso\\.", "association", t)
          t <- gsub("ins\\.", "insurance", t)
          t <- gsub("util\\.", "utility", t)
          t <- gsub("e\\.", "east", t)
          t <- gsub("w\\.", "west", t)
          t <- gsub("n\\.", "north", t)
          t <- gsub("s\\.", "south", t)
          t <- gsub("st\\.", "street", t)
          t <- gsub("nat'l", "national", t)
          t <- gsub("r\\.r\\.", "railroad", t)
          t <- gsub("sys\\.", "system", t)
          t <- gsub("mfg\\.", "manufacturing", t)
}
casea[,"ttl"] <- apply(as.matrix(casea[,"casetitlelexisnexis"]), 1, normttl)
caseb[,"ttl"] <- apply(as.matrix(caseb[,"casetitlelexisnexis"]), 1, normttl)

# Join on court, date, and title
x <- merge(casea, caseb, by=c("courtid", "decisiondate", "ttl"), all.x=T)[,c("courtid", "decisiondate", "ttl", "lni.x", "lni.y", "nj")]

# Aggregate cases with panel by each case without panel
y <- aggregate(1:nrow(x), by=list(x[,"lni.x"]),
               function(k) {
                 if(length(which(is.na(x[k,"lni.y"])))==0) {
                   length(k)
                  } else {
                    0
                  }
               })

# Tabulate frequency of panel cases for no-panel cases
table(y[,"x"])

# Tabulate number of judges
z <- as.matrix(table(x[,"nj"]))

# Plot proportion cases with no panel by court and year (total cases and cases with duplicate panel case)

# Total cases by court and year
tnc <- dbGetQuery(db, "select courtid, year(decisiondate) as year, count(1) as n from CaseHeader group by courtid, year(decisiondate)")

# Cases with no panel
z0 <- aggregate(rep(1, nrow(x)), by=list(x[,"courtid"], as.integer(substring(x[,"decisiondate"], 1, 4))), sum)
colnames(z0) <- c("Court", "Year", "n")
z0 <- merge(z0, tnc, by.x=c("Court", "Year"), by.y=c("courtid", "year"))

# Cases with no panel, but are duplicated by a case with panel, by court and year
k <- which(!is.na(x[,"lni.y"]))
z1 <- aggregate(rep(1, length(k)), by=list(x[k,"courtid"], as.integer(substring(x[k,"decisiondate"], 1, 4))), sum)
colnames(z1) <- c("Court", "Year", "n")
z1 <- merge(z1, tnc, by.x=c("Court", "Year"), by.y=c("courtid", "year"))

ggplot() +
  geom_line(data=z0, aes(x=Year, y=n.x/n.y, linetype="missing_panel")) +
  geom_line(data=z1, aes(x=Year, y=n.x/n.y, linetype="missing_panel_with_duplicate_panel_case")) +
  scale_linetype_manual(name="", values=c("missing_panel"="solid", "missing_panel_with_duplicate_panel_case"="dashed")) +
  geom_text(data=setNames(aggregate(tnc[,"n"], by=list(tnc[,"courtid"]), sum), c("Court", "n")),
            aes(x=1978, y=0.85, label=paste(format(n, big.mark=","), " total case(s)", sep="")),
            size=2.5, hjust=0) +
  facet_wrap(~Court) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        #panel.spacing=unit(-0.2, "lines"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=8),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
 labs(title="Distribution of No-Panel Cases\n", x="\nyear", y="proportion of total cases\n")

z <- dbGetQuery(db, "select * from Court")[,1:2]


#######################################################################################
#### Report data available for cases that are missing a decision date or court ID
#### Report data available for cases with panels containing a judge with missing name
#######################################################################################

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review")

# Missing decision date
x <- dbGetQuery(db,
       " select ch.LNI, ct.ShortName as Court, ch.DecisionDate, ch.CaseTitleLexisNexis,
                ifnull(pn.JudgeID, 'none') as Judge
         from   CaseHeader ch join Court ct on ch.CourtID=ct.ID
                left join Panel pn on ch.lni=pn.lni
                left join Judge jd on pn.JudgeID=jd.ID
         where  ch.DecisionDate is null")
write.table(x, "x.csv", row.names=F, sep="\t", quote=F)

# Missing court
x <- dbGetQuery(db,
       " select ch.LNI, ct.ShortName as Court, ifnull(ch.DecisionDate, '') as DecisionDate,
                ch.CaseTitleLexisNexis, ifnull(jd.Name, 'none') as Judge
         from   CaseHeader ch left join Court ct on ch.CourtID=ct.ID
                left join Panel pn on ch.lni=pn.lni
                left join Judge jd on pn.JudgeID=jd.ID
         where  ifnull(ct.ShortName, '')=''")
y <- aggregate(x[,"Judge"], by=list(x[,"LNI"], x[,"Court"], x[,"DecisionDate"], x[,"CaseTitleLexisNexis"]),
               function(j) paste(unique(j), collapse=", ", sep=""))
write.table(y, "x.csv", row.names=F, sep="\t", quote=F)

# Missing judge name
x <- dbGetQuery(db,
       " select ch.LNI, ct.ShortName as Court, ifnull(ch.DecisionDate, '') as DecisionDate,
                ch.CaseTitleLexisNexis, ifnull(pn.JudgeID, 'none') as JudgeID,
                ifnull(jd.Name, 'none') as JudgeName
         from   CaseHeader ch left join Court ct on ch.CourtID=ct.ID
                left join Panel pn on ch.lni=pn.lni
                left join Judge jd on pn.JudgeID=jd.ID
         where  ifnull(jd.Name, '')=''")


#######################################################################################
#### Enumerate citations by year
#######################################################################################

x <- dbGetQuery(db1,
                "select   case when(ci.lni is not null)then year(cs.decisiondate) else 0 end as year, count(1)
                 from     CaseHeader cs left join Citation ci on cs.lni=ci.lni
                 group by case when(ci.lni is not null)then year(cs.decisiondate) else 0 end")
print(x, row.names=F)


#######################################################################################
#### Identify cases that appeared in the 2019-03-13 data that do not appear in the
#### 2019-12-03 data and vice-versa
#######################################################################################

x <- dbGetQuery(db1,
                "select a.LNI, a.DecisionDate, a.CaseTitleShort, b.ShortName as Court
                 from   CaseHeader a left join Court b on a.CourtID=b.ID
                        left join Appeals2.CaseHeader c on a.LNI=c.LNI
                 where  c.LNI is null")
nrow(x)
write.table(x, "MissingLNI-A-B.csv", col.names=T, row.names=F, sep=",", quote=T)

x <- dbGetQuery(db2,
                "select a.LNI, a.DecisionDate, a.CaseTitleShort, b.ShortName as Court
                 from   CaseHeader a left join Court b on a.CourtID=b.ID
                        left join Appeals.CaseHeader c on a.LNI=c.LNI
                 where  c.LNI is null")
nrow(x)
write.table(x, "MissingLNI-B-A.csv", col.names=T, row.names=F, sep=",", quote=T)

# Enumerate cases by year and court
x <- dbGetQuery(db2,
                "select   year(a.DecisionDate) as year, b.ShortName as Court, count(1) as n
                 from     CaseHeader a left join Court b on a.CourtID=b.ID
                          left join Appeals.CaseHeader c on a.LNI=c.LNI
                 where    c.LNI is null
                 group by year(a.DecisionDate), b.ShortName")
write.table(x, "MissingLNI-B-A-Year-Court.csv", col.names=T, row.names=F, sep=",", quote=T)


#######################################################################################
#### Miscellaneous
#######################################################################################

dbGetQuery(db1, "select count(1) from CaseHeader where LNI='3S4W-YTC0-003B-G17H-00000-00'")
dbGetQuery(db2, "select count(1) from CaseHeader where LNI='3S4W-YTC0-003B-G17H-00000-00'")
dbGetQuery(db1, "select * from CaseLegalTopics where LNI='3S4W-YTC0-003B-G17H-00000-00'")
dbGetQuery(db2, "select * from CaseLegalTopics where LNI='3S4W-YTC0-003B-G17H-00000-00'")
dbGetQuery(db1, "select count(1) from CaseLegalTopics")
dbGetQuery(db1, "select count(2) from CaseLegalTopics")

dbGetQuery(db1, "select * from Judge")


#######################################################################################
#### Disconnect from database 
#######################################################################################

dbDisconnect(db1)
dbDisconnect(db2)

