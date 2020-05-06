# Duke University Law Appeals Analysis
# Review of 2019-03-13 LexisNexi Data

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(ggplot2)
library(xtable)

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-03-13")
imgdir <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\images"


#######################################################################################
#### Connect to Appeals database
#######################################################################################

#install.packages("DBI")
#install.packages("RMySQL")
library(DBI)
library(RMySQL)

# Connect to Appeals database
if(substring(R.Version()[["os"]], 1, 5)=="mingw") {
  # Use ssh port 3306 forwarding from clients
  # Mac:  ssh -L 3306:127.0.0.1:3306 user@lexnex-smben-01.oit.duke.edu
  # Windows:  Install PuTTY and configure a connection for Host Name lexnex-smben-01.oit.duke.edu
  #           Under ssh, Source port=3306, Destination=127.0.0.1:3306
  db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals",
                  # showPrompt() generates error on v 3.4.2
                  #user=rstudioapi::showPrompt("Duke Law Appeals", "User ID:  ", Sys.info()["user"]),
                  user="tjb48",
                  password=rstudioapi::askForPassword("Password:  "))
} else {
  # Local connection
  db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals", user="tjb48", password="")
}

#######################################################################################
#### List tables
#######################################################################################

dbGetQuery(db, "show tables")
dbGetQuery(db, "describe CaseHeader")
dbGetQuery(db, "describe Court")

#######################################################################################
#### Tabulate cases by year 
#######################################################################################

x <- dbGetQuery(db, "select year(DecisionDate), count(1) as n from CaseHeader group by year(DecisionDate)")
sum(x[,"n"])

#######################################################################################
#### Test for duplicate LNI in case headers 
#######################################################################################

x <- dbGetQuery(db, "select LNI, count(1) as n from CaseHeader group by LNI having count(1)>1")

#######################################################################################
#### Tabulate cases by type 
#######################################################################################

# Test for duplicate case, type combinations
x <- dbGetQuery(db, "select LNI, CaseType, count(1) as n from CaseType group by LNI, CaseType having count(1)>1") 

# Enumerate types
dbGetQuery(db, "select distinct CaseType from CaseType") 

# Aggregate cases by year and type
# R warnings may be generated here
# They can be suppressed with suppressWarnings(), but then all warnings are concealed
x <- dbGetQuery(db,
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

#dev.new()
y <- data.frame("year"=x[,"year"],
                "type"=c(rep("criminal", nrow(x)), rep("civil", nrow(x)), rep("crim-civ", nrow(x)), rep("none", nrow(x))),
                "n"=c(x[,"nCriminal"], x[,"nCivil"], x[,"nCrimCiv"], x[,"nNone"]))

#dev.new()
png(paste(imgdir, "\\YearCaseTypeDensity.png", sep=""), res=300, width=2400, height=2400)
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

x <- dbGetQuery(db, "select a.LNI from CaseHeader a left join Panel b on a.LNI=b.LNI where b.LNI is null")

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
#### List cases with no legal topics
#######################################################################################

x <- dbGetQuery(db,
       "select LNI
        from   CaseHeader
        where  LNI not in(select distinct LNI from CaseLegalTopics)")
length(x)

#######################################################################################
#### Tabulate number of legal topics
#######################################################################################

x <- dbGetQuery(db,
       "select   case when(b.n is not null)then b.n else 0 end as nTopic, count(1) as nCase
        from     CaseHeader a
                 left join(select   LNI, count(1) as n
                           from     CaseLegalTopics
                           group by LNI) b on a.LNI=b.LNI
        group by b.n")

png(paste(imgdir, "\\CaseLegalTopicDistribution.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_line(data=x, aes(x=nTopic, y=log(nCase)/log(10))) +
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
  labs(x="\nnumber of legal topics", y="number of cases (log-10)\n")
dev.off()

#######################################################################################
#### List cases with no LN topics
#######################################################################################

x <- dbGetQuery(db,
       "select LNI
        from   CaseHeader
        where  LNI not in(select distinct LNI from CaseLNTopics)")
nrow(x)

#######################################################################################
#### Tabulate number of LN topics
#######################################################################################

x <- dbGetQuery(db,
       "select   case when(b.n is not null)then b.n else 0 end as nTopic, count(1) as nCase
        from     CaseHeader a
                 left join(select   LNI, count(1) as n
                           from     CaseLNTopics
                           group by LNI) b on a.LNI=b.LNI
        group by b.n")

ggplot() +
  geom_line(data=x, aes(x=nTopic, y=log(nCase)/log(10))) +
  scale_y_continuous(breaks=1:6, labels=format(10**(1:6), big.mark=",")) +
  labs(x="\nnumber of LN topics", y="number of cases\n")

#######################################################################################
#### List cases with different number of legal and LN topics
#######################################################################################

x <- dbGetQuery(db,
       "select a.LNI
        from   CaseHeader a
               left join(select   LNI, count(1) as n
                         from     CaseLegalTopics
                         group by LNI) b on a.LNI=b.LNI
               left join(select   LNI, count(1) as n
                         from     CaseLNTopics
                         group by LNI) c on a.LNI=c.LNI
        where  case when(b.n is not null)then b.n else 0 end <> case when(c.n is not null)then c.n else 0 end")

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
#### Disconnect from database 
#######################################################################################

dbDisconnect(db)
