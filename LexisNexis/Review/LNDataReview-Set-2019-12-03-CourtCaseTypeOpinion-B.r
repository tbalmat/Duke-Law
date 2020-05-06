# Duke University Law Appeals Analysis
# Review of 2019-12-03 LexisNexis Data
# Followup issues to report, resulting from 2019-04-16 team meeting

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(ggplot2)
#library(xtable)
library(DBI)
library(RMySQL)

#######################################################################################
# Set directories
#######################################################################################

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-04-20")
lnsourcedir1 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-03-13"
lnsourcedir2 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03"
imgdir <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-04-20"


#######################################################################################
# Connect to Appeals databases
# db1 for data set 1, March 2019
# db2 for data set 2, December 2019
#######################################################################################

usr <- "tjb48"
#dbDisconnect(db1)
#dbDisconnect(db2)
db1 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals", user=usr, password=rstudioapi::askForPassword("Password:  "))
db2 <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals2", user=usr, password=rstudioapi::askForPassword("Password: "))

#######################################################################################
# List table structures
#######################################################################################

dbGetQuery(db1, "show tables")
dbGetQuery(db2, "show tables")
dbGetQuery(db1, "describe CaseHeader")
dbGetQuery(db2, "describe CaseHeader")
dbGetQuery(db2, "describe CaseHeaderExt")
dbGetQuery(db1, "describe CaseType")
dbGetQuery(db2, "describe CaseType")
dbGetQuery(db1, "describe Court")
dbGetQuery(db2, "describe Court")
dbGetQuery(db1, "describe CaseOutcomeType")
dbGetQuery(db2, "describe CaseOutcomeType")
dbGetQuery(db1, "describe Opinion")
dbGetQuery(db2, "describe Opinion")
dbGetQuery(db2, "select distinct opiniontype from Opinion")
dbGetQuery(db2, "select * from Court")
dbGetQuery(db2, "describe CaseLegalTopics")
dbGetQuery(db2, "select distinct pubstatus from CaseHeader")

######################################################################################
# Table and field examination
#######################################################################################

# Verify one-one case header extension existence
dbGetQuery(db2, "select count(1) from CaseHeader where lni not in(select lni from CaseHeaderExt)")

# Evaluate char length of null (returns null)
dbGetQuery(db2, "select character_length(ifnull(null, ''))")

#######################################################################################
# Plot the distributions of opinion text length for 4th and 11th circuits 
#######################################################################################

# Accumulate text lengths by opinion type
# Note that each case header case has a corresponding extension record

x <- dbGetQuery(db2, "select c.ShortName as court,
                             year(a.DecisionDate) as year, 
                             character_length(ifnull(b.OpinionByText, '')) as no,
                             character_length(ifnull(b.ConcurByText, '')) as nc,
                             character_length(ifnull(b.DissentByText, '')) as nd
                      from   CaseHeader a join CaseHeaderExt b on a.lni=b.lni 
                             join Court c on a.CourtID=c.ID
                      where  c.ShortName in('4th Circuit Court of Appeals',
                                            '11th Circuit Court of Appeals')")

# Abbreviate court names
unique(x[,"court"])
x[,"court"] <- sub("Circuit Court of Appeals", "Circuit", x[,"court"])
#x[,"court"] <- factor(x[,"court"], levels=c("4th Circuit", "11th Circuit"))   

# Plot annual panels of distributions by court

ct <- c("4th Circuit", "11th Circuit")[2]
if(ct=="4th Circuit") {
  f <- paste(imgdir, "\\images\\Dist-4th-OpText-Length.png", sep="")
} else {
  f <- paste(imgdir, "\\images\\Dist-11th-OpText-Length.png", sep="")
}

# Filter by court and text length
nlim <- 50
ko <- which(x[,"court"]==ct & x[,"no"]>0 & x[,"no"]<=nlim)
kc <- which(x[,"court"]==ct & x[,"nc"]>0 & x[,"nc"]<=nlim)
kd <- which(x[,"court"]==ct & x[,"nd"]>0 & x[,"nd"]<=nlim)

nbins <- c(30)[1]

gdat <- rbind(data.frame("type"="opinion", "year"=x[ko,"year"], "n"=x[ko, "no"]),
              data.frame("type"="concur", "year"=x[kc,"year"], "n"=x[kc, "nc"]),
              data.frame("type"="dissent", "year"=x[kd,"year"], "n"=x[kd, "nd"]))
gdat[,"type"] <- factor(gdat[,"type"], levels=c("dissent", "concur", "opinion"))

png(f, res=300, width=2400, height=2400)
ggplot() +
  geom_histogram(data=gdat, aes(x=n, fill=type, group=type), alpha=1, position="stack", bins=nbins) +
  scale_fill_manual(values=c("opinion"="green", "concur"="blue", "dissent"="red")) +
  #scale_x_continuous(breaks=log(seq(10, 100, 10))/log(10), labels=seq(10, 100, 10)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  scale_y_log10(labels=function(x) format(x, big.mark=",")) +
  facet_wrap(~year) +
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
  labs(x="\ntext length", y="number of cases\n")
dev.off()

# Review opinion bys of length between 5 and 15 chars in years 1990-1994
y <- dbGetQuery(db2, "select c.ShortName as court,
                             year(a.DecisionDate) as year,
                             b.OpinionByText,
                             character_length(ifnull(b.OpinionByText, '')) as no
                      from   CaseHeader a join CaseHeaderExt b on a.lni=b.lni 
                             join Court c on a.CourtID=c.ID
                      where  c.ShortName in('4th Circuit Court of Appeals',
                                            '11th Circuit Court of Appeals')
                             and year(a.DecisionDate) between 1990 and 1994
                             and character_length(ifnull(b.OpinionByText, '')) between 5 and 15")

# Plot proportion of opinion-by text fields containing the exact text "per curiam"
# Panel by court and year

z <- dbGetQuery(db2, "select   c.ShortName as court, year(a.DecisionDate) as year,
                               sum(case when(lower(b.OpinionByText)='per curiam')then 1 else 0 end)*1./count(1) as p
                      from     CaseHeader a join CaseHeaderExt b on a.lni=b.lni 
                               join Court c on a.CourtID=c.ID
                      group by c.ShortName, year(a.DecisionDate)")

# Abbreviate names
unique(x[,"court"])
z[,"court"] <- sub("Circuit Court of Appeals", "Circ", z[,"court"])
z[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", z[,"court"])
z[,"court"] <- sub("Court of Federal Claims", "Fed Claims", z[,"court"])
z[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", z[,"court"])
z[,"court"] <- sub("Judicial Conference, Committee on Judicial Conduct", "Jud Conduct", z[,"court"])
z[,"court"] <- sub("Temporary Emergency Court of Appeals", "Temp Emergency", z[,"court"])
z[,"court"] <- sub("Tennessee Eastern District Court", "Tenn E Dist", z[,"court"])
z[,"court"] <- sub("Texas Southern District Court", "Tex S Dist", z[,"court"])
sort(unique(z[,"court"]))
z[,"court"] <- factor(z[,"court"], levels=c("", "1st Circ", "2nd Circ", "3rd Circ", "4th Circ", "5th Circ",
                                            "6th Circ", "6th Circ Bkruptcy", "7th Circ", "8th Circ",
                                            "9th Circ", "10th Circ", "11th Circ", "DC Circ", "Fed Claims",
                                            "Federal Circ", "Jud Conduct", "Temp Emergency", "Tenn E Dist",
                                            "Tex S Dist"))

png(paste(imgdir, "\\images\\Proportion-Per-Curiam-Opinions-By-Author.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  #geom_rect(data=data.frame(xmin=1990, xmax=1994, ymin=0, ymax=1),
  #          aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="gray85", fill=NA) +
  geom_vline(xintercept=1990, color="gray75", linetype="dashed") +
  geom_vline(xintercept=1994, color="gray75", linetype="dashed") +
  geom_line(data=z, aes(x=year, y=p)) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
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
  labs(x="\nyear", y="proportion \"per curiam\" opinions\n")
dev.off()

# Compare proportion per curiam cases with that of cases containing "per curiam" in opinion author field

z <- dbGetQuery(db2, "select   c.ShortName as court, year(a.DecisionDate) as year,
                               sum(case when(lower(b.OpinionByText)='per curiam')then 1 else 0 end)*1./count(1) as p1,
                               sum(case when(a.PerCuriam=1)then 1 else 0 end)*1./count(1) as p2
                      from     CaseHeader a join CaseHeaderExt b on a.lni=b.lni 
                               join Court c on a.CourtID=c.ID
                      group by c.ShortName, year(a.DecisionDate)")

# Abbreviate names
unique(z[,"court"])
z[,"court"] <- sub("Circuit Court of Appeals", "Circ", z[,"court"])
z[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", z[,"court"])
z[,"court"] <- sub("Court of Federal Claims", "Fed Claims", z[,"court"])
z[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", z[,"court"])
z[,"court"] <- sub("Judicial Conference, Committee on Judicial Conduct", "Jud Conduct", z[,"court"])
z[,"court"] <- sub("Temporary Emergency Court of Appeals", "Temp Emergency", z[,"court"])
z[,"court"] <- sub("Tennessee Eastern District Court", "Tenn E Dist", z[,"court"])
z[,"court"] <- sub("Texas Southern District Court", "Tex S Dist", z[,"court"])
sort(unique(z[,"court"]))
z[,"court"] <- factor(z[,"court"], levels=c("", "1st Circ", "2nd Circ", "3rd Circ", "4th Circ", "5th Circ",
                                            "6th Circ", "6th Circ Bkruptcy", "7th Circ", "8th Circ",
                                            "9th Circ", "10th Circ", "11th Circ", "DC Circ", "Fed Claims",
                                            "Federal Circ", "Jud Conduct", "Temp Emergency", "Tenn E Dist",
                                            "Tex S Dist"))

png(paste(imgdir, "\\images\\Proportion-Per-Curiam-Opinions-By-PerCuriam.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_vline(xintercept=1990, color="gray75", linetype="dashed") +
  geom_vline(xintercept=1994, color="gray75", linetype="dashed") +
  geom_line(data=z, aes(x=year, y=p1, linetype="author")) +
  geom_line(data=z, aes(x=year, y=p2, linetype="per-curiam-indicator")) +
  scale_linetype_manual(name="method", values=c("author"="solid", "per-curiam-indicator"="dashed")) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
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
  labs(x="\nyear", y="proportion \"per curiam\" opinions\n")
dev.off()

#######################################################################################
# Distribution of proportion cases with outcome type of "other" by court and year
#######################################################################################

# Verify that each case has an outcome type record
dbGetQuery(db2, "select count(1) from CaseHeader where lni not in(select lni from CaseOutcomeType)")

# Inspect outcome type values
dbGetQuery(db2, "select distinct outcometype from CaseOutcomeType")

# Test for null outcome text
dbGetQuery(db2, "select count(1) from CaseHeader where outcome is null")

# Compute proportion of "other" cases and proportion "other" cases with empty outcome fields
x <- dbGetQuery(db2, "select   c.ShortName as court, year(a.DecisionDate) as year,
                               sum(case when(b.outcometype='other')then 1 else 0 end)*1./count(1) as p1,
                               sum(case when(a.outcome is null and b.outcometype='other')then 1 else 0 end)*1./
                               sum(case when(b.outcometype='other')then 1 else 0 end) as p2,
                               count(1)*1./d.n as p3
                      from     CaseHeader a join CaseOutcomeType b on a.lni=b.lni 
                               join Court c on a.CourtID=c.ID
                               join( select   year(decisiondate) as year, count(1) as n
                                     from     CaseHeader
                                     group by year(decisiondate)
                               ) d on year(a.decisiondate)=d.year
                      group by c.ShortName, year(a.DecisionDate)")

# Abbreviate names
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

if(T) {
  x[,"y1"] <- x[,"p1"]
  x[,"y2"] <- x[,"p2"]
  # The space in " proportion-total...." is intentional - it places that category first in the legend
  ltlab <- c(" proportion-total-cases-other", "proportion-other-cases-empty")
  png(paste(imgdir, "\\images\\Proportion-Outcome-Other-Empty-Text.png", sep=""), res=300, width=2400, height=2400)
} else {
  x[,"y1"] <- 1-x[,"p1"]
  x[,"y2"] <- 1-x[,"p2"]
  # The space in " proportion-total...." is intentional - it places that category first in the legend
  ltlab <- c(" proportion-total-cases-non-other", "proportion-other-cases-non-empty")
  png(paste(imgdir, "\\images\\Proportion-Outcome-Other-Empty-Text-Neg.png", sep=""), res=300, width=2400, height=2400)
}

ggplot() +
  geom_line(data=x, aes(x=year, y=y1, linetype=ltlab[1])) +
  geom_line(data=x, aes(x=year, y=y2, linetype=ltlab[2])) +
  geom_bar(data=x, aes(x=year, y=p3), stat="identity", color="blue3", fill=NA) +
  scale_linetype_manual(name="", values=setNames(c("solid", "dashed"), ltlab)) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
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
  labs(x="\nyear", y="proportion\n")

dev.off()

# Randomly sample outcome text
x <- dbGetQuery(db2, "select b.ShortName as court, year(a.decisiondate) as year, a.outcome
                      from   CaseHeader a join Court b on a.CourtID=b.ID
                             join CaseOutcomeType c on a.lni=c.lni
                      where  c.outcometype='other'")
x[,"court"] <- sub("Circuit Court of Appeals", "Circ", x[,"court"])
x[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", x[,"court"])
x[,"court"] <- sub("Court of Federal Claims", "Fed Claims", x[,"court"])
x[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", x[,"court"])
x[,"court"] <- sub("Judicial Conference, Committee on Judicial Conduct", "Jud Conduct", x[,"court"])
x[,"court"] <- sub("Temporary Emergency Court of Appeals", "Temp Emergency", x[,"court"])
x[,"court"] <- sub("Tennessee Eastern District Court", "Tenn E Dist", x[,"court"])
x[,"court"] <- sub("Texas Southern District Court", "Tex S Dist", x[,"court"])

# Text containing "dismiss"
k <- sample(grep("dismiss", x[,"outcome"]), 50, replace=F)
k <- k[order(x[k,"year"])]
writeLines(paste(x[k,"court"], " & ", x[k,"year"], " & ", gsub("\\$", "\\\\$", x[k,"outcome"]), "\\\\[4pt]", sep=""))  

# 1993-2000, text does not contain "dismiss"
k <- sample(intersect(which(x[,"year"]>1992 & x[,"year"]<2001 & !is.na(x[,"outcome"])),
                      grep("dismiss", x[,"outcome"], invert=T)),
            100, replace=F)
k <- k[order(x[k,"year"])]
writeLines(paste(x[k,"court"], " & ", x[k,"year"], " & ", gsub("\\$", "\\\\$", x[k,"outcome"]), "\\\\[4pt]", sep=""))  

#######################################################################################
# Identify duplicate records for cases with alternative spellings of titles in their names 
#######################################################################################

# Sample short, long, and LN case titles
x <- dbGetQuery(db2, "select casetitleshort, casetitlelong, casetitlelexisnexis from CaseHeader")

# Render a sample of titles in Latex
a <- ""
for(i in sample(1:nrow(x), 10, replace=F))
  a <- c(a,
         paste(gsub("&", "\\&", gsub("$", "\\$", x[i,"casetitleshort"], fixed=T), fixed=T), " & ",
               gsub("&", "\\&", gsub("$", "\\$", x[i,"casetitlelong"], fixed=T), fixed=T), " & ",
               gsub("&", "\\&", gsub("$", "\\$", x[i,"casetitlelexisnexis"], fixed=T), fixed=T), " &\\\\", sep=""),
         "& & &\\\\[-4pt]")  
writeLines(a)

# Retrieve case data
x <- dbGetQuery(db2, "select a.lni, a.casetitleshort, year(a.decisiondate) as year, b.shortname as court
                      from   CaseHeader a join Court b on a.courtid=b.id")

# Compose abbreviation substitutions
# Note that the following "words" should be surrounded by spaces when searching text for accurate delimiting
tsub <- matrix(c(
"u.s.", "united states",
"usa ", "united states",
"us", "united states",
"NLRB", "national labor relations board",
"e.e.o.c.", "equal employment opportunity commission",
"o.w.c.p.", "office of workers' compensation programs",
"sec. dep't of", "secretary department of",
"sec., dep't of", "secretary, department of",
"social sec.", "social security",
"employment sec.", "employment security",
"nat'l comm.", "national committee",
"aclu", "american civil liberties union",
"afscme", "american federation of state, county and municipal employees",
"batf", "bureau of alcohol tobacco and firearms",
"cia", "central intelligence agency",
"faa", "federal aviation administration",
"fbi", "federal bureau of investigation",
"fdic", "federal deposit insurance corporation",
"fha", "federal housing authority",
"frb", "federal reserve board",
"hud", "housing and urban development",
"ins", "immigration and naturalization service",
"irs", "internal revenue service",
"naacp", "national association for the advancement of colored people",
"nra", "national rifle association",
"nrc", "nuclear regulatory commission",
"nrdc", "natural resources defense council",
"ntsb", "national transportation safety council",
"nyse", "new york stock exchange",
"omb", "office of management and budget",
"opm", "office of personnel management",
"osha", "occupational safety and health administration",
"pbs", "public broadcasting service",
"sba", "small business administration",
"ssa", "social security administration",
"stb", "surface transportation board",
"uaw", "united auto workers",
"ufcw", "united food and commercial workers",
"ufw", "united farm workers",
"ups", "united parcel service",
"usda", "united states department of agriculture",
"usps", "united states postal service",
" va ", "united states department of veterans affairs",
"ag's", "attorney general's",
"admin'r", "administrator",
"adm'r", "administrator",
"ass'n", "association",
"assn's", "associations",
"att'y", "attorney",
"atty's", "attorneys",
"c'mmr", "commissioner",
"comm'n", "commission",
"comm'r", "commissioner",
"com'n", "commission",
#"comn'n", "commonwealth",
"com'r", "commissioner",
"commr's", "commissioners",
"comn'r", "commissioner",
"comr's", "commissioners",
"cont'l", "continental",
"da's", "district attorney's",
"dep't", "department",
"enf't", "enforcement",
"emplr's'.", "employers'",
"emples'.", "employees'",
"emples.'", "employees'",
"eng'g", "engineering",
"eng'r", "engineer",
"entm't", "entertainment",
"env't", "environment",
"exam'r", "examiner",
"ex'r", "examiner",
"examr's", "examiner's",
"fed'n", "federation",
"fla.'s", "florida's",
"gen'l", "general",
"gen's", "general's",
"gen.'s", "general's",
"gov't", "government",
"govn't", "government",
"indp't", "independent",
"inter'l", "international",
"int'l", "international",
"intern'l", "international",
"intern'l.", "international",
"intrn'l", "international",
"inv'rs", "investors",
"mem'l", "memorial",
"mem'l.", "memorial",
"mfr.'s", "manufacturer's",
"na'l", "national",
"nat'l", "national",
"nt'l", "national",
"p'ship", "partnership",
"p'shp", "partnership",
"p'shp.", "partnership",
"prof'l", "professional",
"publ'g", "publishing",
"publ'n", "publishing",
"publ'ns.", "publications",
"publ'ns", "publications",
"publ'rs", "publishers",
"reg'l", "regional",
"sec't", "secretary",
"sec'y", "secretary",
"s'holders", "shareholders",
"sup'r", "supervisor",
"soc'y", "society",
"acc.", "accident",
"acci.", "accident",
"admin.", "administration",
"adver.", "advertizing",
"agric.", "agriculture",
"ala.", "alabama",
"am.", "american",
"appt.", "apartment",
"ariz.", "arizona",
"ark.", "arkansas",
"assn.", "association",
"asso.", "association",
"assoc.", "association",
"assocs.", "associations",
"assur.", "assurance",
"atty.", "attorney",
"attys.", "attorneys",
"auth.", "authority",
"auto.", "automotive",
"ave.", "avenue",
"balt.", "baltimore",
"bankr.", "bankruptcy",
"bhd.", "brotherhood",
"bldg.", "building",
"bldgs.", "buildings",
"bros.", "brothers",
"broth.", "brothers",
"bus.", "business",
"cal.", "california",
"chem.", "chemical",
"chems.", "chemicals",
"chgo.", "chicago",
"chi.", "chicago",
"civ.", "civil",
"cmty.", "community",
"cnty.", "county",
"co.", "company",
"cos.", "companies",
"colo.", "colorado",
"com.", "commission",
"commer.", "commercial",
"commn.", "commission",
"commun.", "communication",
"communs.", "communications",
"comp.", "compensation",
"condo.", "condominium",
"conn.", "connecticut",
"consol.", "consolidated",
"const.", "construction",
"constr.", "construction",
"contr.", "contractor",
"contrs.", "contractors",
"coop.", "cooperative",
"coops.", "cooperatives",
"corp.", "corporation",
"corr.", "correction",
"crim.", "criminal",
"ctr.", "center",
"ctrs.", "centers",
"cty.", "city",
"def.", "defense",
"del.", "delaware",
"dept.", "department",
"dev.", "development",
"det.", "detention",
"dir.", "director",
"disc.", "discipline",
"discrim.", "discrimination",
"dist.", "district",
"distrib.", "distribution",
"distribs.", "distributors",
"div.", "division",
"econ.", "economic",
"educ.", "education",
"elec.", "electric",
"elecs.", "electronics",
"emples.", "employees",
"emplr.", "employer",
"emplrs.", "employers",
"enter.", "enterprise",
"enters.", "enterprises",
"envtl.", "environmental",
"equal.", "equality",
"equip.", "equipment",
"exch.", "exchange",
"exec.", "executive",
"exp.", "export",
"fed.", "federal",
"fedn.", "federation",
"fid.", "fidelity",
"fin.", "finance",
"fla.", "florida",
"found.", "foundation",
"ga.", "georgia",
"gen.", "general",
"grp.", "group",
"guar.", "guarantee",
"hon.", "honorable",
"hosp.", "hospital",
"hosps.", "hospitals",
"hous.", "houston",
"ill.", "illinois",
"imp.", "import",
"imps.", "importers",
"inc.", "incorporated",
"indem.", "indemnity",
"indus.", "industry",
"info.", "information",
"ins.", "insurance",
"inst.", "institute",
"intern.", "international",
"intl.", "international",
"inv.", "investment",
"invest.", "investment",
"invs.", "investments",
"kan.", "kansas",
"ky.", "kentucky",
"la.", "lousiana",
"lab.", "laboratory",
"labs.", "laboratories",
"liab.", "liability",
"litig.", "litigation",
"ltd.", "limited",
"mach.", "machine",
"maint.", "maintenance",
"md.", "maryland",
"me.", "maine",
"mech.", "mechanical",
"med.", "medical",
"mem.", "memorial",
"merch.", "merchant",
"metro.", "metropolitan",
"mfg.", "manufacturing",
"mfrs.", "manufacturers",
"mgmt.", "management",
"mich.", "michigan",
"minn.", "minnesota",
"miss.", "mississippi",
"mkt.", "market",
"mktg.", "marketing",
"mkts.", "markets",
"mo.", "missouri",
"mont.", "montana",
"mortg.", "mortgage",
"mr.", "mister",
"mun.", "municipal",
"mut.", "mutual",
"n.c.", "north carolina",
"n.h.", "new hampshire",
"n.j.", "new jersey",
"n.m.", "new mexico",
"n.y.", "new york",
"natl.", "national",
"nev.", "nevada",
"no.", "number",
"new eng.", "new england",
"ofc.", "office",
"off.", "office",
"okla.", "oklahoma",
"or.", "oregon",
"org.", "organization",
"pa.", "pennsylvania",
"pac.", "pacific",
"par.", "parish",
"pers.", "personnel",
"pharm.", "pharmaceutical",
"pharms.", "pharmaceuticals",
"phila.", "philadelphia",
"reprod.", "reproductive",
"prod.", "product",
"prods.", "products",
"prop.", "property",
"props.", "properties",
"prot.", "protection",
"pshp.", "partnership",
"pub.", "public",
"publ.", "publishing",
"publs.", "publishers",
"r.i.", "rhode island",
"rd.", "road",
"rds.", "roads",
"rec.", "recreation",
"rehab.", "rehabilitation",
"rels.", "relations",
#"res.", "resources",
"rest.", "restaurant",
"rests.", "restaurants",
"ret.", "retirement",
"rev.", "revenue",
"ry.", "railway",
"s.c.", "south carolina",
"s.d.", "south dakota",
"sch.", "school",
"schs.", "schools",
"soc. sec.", "social secutity",
"homeland sec.", "homeland security",
"sec. for", "secretary for",
"sec. of", "secretary of",
"serv.", "service",
"servs.", "services",
"std.", "standard",
"sys.", "system",
"tel.", "telephone",
"tenn.", "tennessee",
"tex.", "texas",
"transp.", "transportation",
"twp.", "township",
"univ.", "university",
"va.", "virginia",
"wash.", "washington"
),
ncol=2, byrow=T)

# Compose Latex table with wrapped columns of text substitution pairs
ncol <- 2
a <- ""
for(i in seq(1, nrow(tsub), ncol)) {
  j <- min(i+ncol-1, nrow(tsub))
  b <- paste(tsub[i:j,1], " & ", tsub[i:j,2], sep="")
  a <- c(a,
         paste(paste(b, collapse=" & & ", sep=""), " &\\\\",  sep=""),
         paste(paste(rep(" & ", 3*ncol-1), collapse="", sep=""), "\\\\[-6pt]", sep=""))
}
writeLines(a)

# Identify words in case titles that contain a special character(s) (apostrophe, period), but are not in the tsub vector
schar <- c("'", "\\.")[2]
y <- sort(unique(
       unlist(lapply(
         # Extract case titles that do not contain words in tsub
         x[-unique(
              unlist(
                lapply(1:nrow(tsub),
                  function(i)
                    grep(tsub[i,1],
                      gsub(" v. ", "", tolower(x[,"casetitleshort"])), fixed=T)
                ))),"casetitleshort"],
         function(a) {
           # Identify words containing an special character(s)
           b <- strsplit(a, " ")[[1]]
           b[grep(schar, b)]
         }))
     ))

# Examine words with specified leading character(s)
writeLines(y[which(tolower(substring(y, 1, 1))=="s")])

# Examine specific strings
x[grep("b.a.s.i.c.", tolower(x[,"casetitleshort"]), fixed=T),]

x[setdiff(setdiff(setdiff(setdiff(
    grep("sec\\.", tolower(x[,"casetitleshort"])),
    grep("soc\\. sec\\.", tolower(x[,"casetitleshort"]))),
    grep("homeland sec\\.", tolower(x[,"casetitleshort"]))),
    grep("sec\\. for", tolower(x[,"casetitleshort"]))),
    grep("sec\\. of", tolower(x[,"casetitleshort"]))),]

# Examine upper case words in titles
z <- sort(unique(unlist(
       lapply(x[,"casetitleshort"],
         function(a) {
           lapply(strsplit(a, " ")[[1]],
             function(b)
               if(nchar(b)==length(grep("[A-Z]", strsplit(b, "")[[1]]))) {
                 return(b)
               } else {
                 return(NULL)
               })
         }))))
writeLines(z[which(tolower(substring(z, 1, 1))=="v")])

####
# Convert title text to lower case
# Surround with spaces so that leading and trailing words are delimited on each side
# Replace commas, colons, and semicolons with a single space (to avoid, for instance, "int'l,")
# Omit repeated spaces
####
ttl <- tolower(
         # Surround with spaces first so that leading and trailing spaces become repeats to be collapsed
         # Convert punctuation symbols to a space then convert repeating spaces
         # Note that the comma is a control character within [], so escape it
         # \s+ locates repeated whitespace, tabs, new line, cr, vert tab
         gsub("\\s+", " ",
              gsub("[\\,;:]", " ", 
                   paste(" ", x[,"casetitleshort"], " ", sep=""))))

# Verify absence of punctuation and repeated spaces
grep(",", ttl)
grep(";", ttl)
grep(":", ttl)
ttl[grep("  ", ttl)]

# Substitute text in titles
# Include delimiting spaces so that "words" are isolated
for(i in 1:nrow(tsub))
  ttl <- gsub(paste(" ", tsub[i,1], " ", sep=""), paste(" ", tsub[i,2], " ", sep=""), ttl, fixed=T)

gc()

# Omit surrounding spaces in substituted titles
which(substring(ttl, 1, 1) != " ")
which(substring(ttl, nchar(ttl), nchar(ttl)) != " ")
ttl <- substring(ttl, 2, nchar(ttl)-1)

# Compare initial and text-substituted titles
w <- cbind(x[,"casetitleshort"], ttl, "")

# Upload text-substituted titles to database
dbGetQuery(db2, "create table CaseAltTitle(LNI varchar(50) primary key, AltTitle varchar(500))")
dbGetQuery(db2, "truncate table CaseAltTitle")
nt <- 10000
for(i in seq(1, nrow(x), nt)) {
  k <- i:min(i+nt-1, nrow(x))
  query <- paste("insert into CaseAltTitle(lni, alttitle) values(",
                 paste(paste("'", x[k,"lni"], "', '", gsub("'", "''", ttl[k], fixed=T), "'", sep=""),
                       collapse="), (", sep=""),
                 ")", sep="")
  dbGetQuery(db2, query)
}

gc()

# Verify case counts
dbGetQuery(db2, "select count(1) from CaseHeader")
dbGetQuery(db2, "select count(1) from CaseAltTitle")
dbGetQuery(db2, "select count(1) from CaseHeader where lni not in(select lni from CaseAltTitle)")

# Enumerate distinct case titles, courts and dates
dbGetQuery(db2, "select count(distinct courtid, decisiondate, casetitleshort) from CaseHeader")
dbGetQuery(db2, "select count(distinct a.courtid, a.decisiondate, b.alttitle) from CaseHeader a join CaseAltTitle b on a.lni=b.lni")

# Verify absence of double spaces in substituted titles
dbGetQuery(db2, "select alttitle from CaseAltTitle where alttitle like '%  %'")

# Identify cases with identical court and decision date, unequal titles, but equal alternate titles
w0 <- dbGetQuery(db2, "select a.lni
                       from   CaseHeader a join CaseHeader b on a.courtid=b.courtid and a.decisiondate=b.decisiondate
                              join Court c on a.courtid=c.id
                              join CaseAltTitle d on a.lni=d.lni
                              join CaseAltTitle e on b.lni=e.lni
                       where  a.casetitleshort<>b.casetitleshort
                              and d.AltTitle=e.AltTitle")

# Tabulate case title duplication by court and year, using original title
z <- dbGetQuery(db2, "select a.lni, a.courtid, b.shortname as court, a.decisiondate, a.casetitleshort, c.n
                      from   CaseHeader a join Court b on a.courtid=b.id
                             join ( select   min(lni) as lni, count(1) as n
                                    from     CaseHeader
                                    group by courtid, decisiondate, casetitleshort
                                    having   count(1)>1
                                  ) c on a.lni=c.lni")
table(z[,"n"])

# Tabulate case title duplication 
z2 <- dbGetQuery(db2, "select a.lni, a.courtid, b.shortname as court, a.decisiondate, a.casetitleshort, c.n
                       from   CaseHeader a join Court b on a.courtid=b.id
                              join ( select   min(a.lni) as lni, count(1) as n
                                     from     CaseHeader a join CaseAltTitle b on a.lni=b.lni
                                     group by a.courtid, a.decisiondate, b.alttitle
                                     having   count(1)>1
                                   ) c on a.lni=c.lni")

table(z2[,"n"])

# Render Latex table to compare distribution of case duplication frequency
z3 <- merge(data.frame(table(z[,"n"])), data.frame(table(z2[,"n"])), by="Var1", all=T)
z3 <- z3[order(as.integer(z3[,"Var1"])),]
writeLines(paste(z3[,1], " & ", format(z3[,2], big.mark=","), " & ", format(z3[,3], big.mark=","), "\\\\", sep=""))

# Abbreviate court names
z2[,"court"] <- sub("Circuit Court of Appeals", "Circ", z2[,"court"])
z2[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", z2[,"court"])
z2[,"court"] <- sub("Court of Federal Claims", "Fed Claims", z2[,"court"])
z2[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", z2[,"court"])
z2[,"court"] <- sub("Judicial Conference, Committee on Judicial Conduct", "Jud Conduct", z2[,"court"])
z2[,"court"] <- sub("Temporary Emergency Court of Appeals", "Temp Emergency", z2[,"court"])
z2[,"court"] <- sub("Tennessee Eastern District Court", "Tenn E Dist", z2[,"court"])
z2[,"court"] <- sub("Texas Southern District Court", "Tex S Dist", z2[,"court"])
z2[,"court"] <- factor(z2[,"court"], levels=c("", "1st Circ", "2nd Circ", "3rd Circ", "4th Circ", "5th Circ",
                                              "6th Circ", "6th Circ Bkruptcy", "7th Circ", "8th Circ",
                                              "9th Circ", "10th Circ", "11th Circ", "DC Circ", "Fed Claims",
                                              "Federal Circ", "Jud Conduct", "Temp Emergency", "Tenn E Dist",
                                              "Tex S Dist"))

# Generate heat map indicating duplicate case frequencies by year and court
# Tabulate duplicate cases by court and year
gdat <- aggregate(z2[,"n"], by=list(z2[,"court"], substring(z2[,"decisiondate"], 1, 4)), sum)
colnames(gdat) <- c("court", "year", "n")
png(paste(imgdir, "\\images\\CaseTitleDuplicateCourtYearHeatMap.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_tile(data=gdat, aes(x=court, y=year, fill=n)) +
  scale_fill_gradient(name="short case name duplicates  ", limits=c(0, 8000), low="#0000b0", high="yellow",
                      labels=function(x) format(x, big.mark=",")) +
  scale_y_discrete(breaks=seq(1974, 2018, 4)) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_rect(fill="#0000b0"),
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
        legend.text=element_text(size=10, angle=90, hjust=0.5, vjust=0.5),
        legend.title=element_text(size=10)) +
  labs(x="\ncourt", y="year\n")
dev.off()

# List cases with identical titles after text substitution
k <- which(z2[,"court"]=="4th Circ" & substring(z2[,"decisiondate"], 1, 4)=="1996" & !z2[,"lni"] %in% z[,"lni"])
k <- k[order(z2[k,"decisiondate"], z2[k,"casetitleshort"])]
w <- z2[k,c("decisiondate", "casetitleshort")]
rownames(w) <- NULL

# Inspect long names and LN names for selected courts and dates (with duplicated short case names)
k2 <-1
w2 <- dbGetQuery(db2, paste(" select   casetitleshort, casetitlelong, casetitlelexisnexis
                              from     CaseHeader
                              where    courtid=", z2[k[k2],"courtid"], " and decisiondate='", z2[k[k2],"decisiondate"], "'",
                            " order by casetitleshort", sep=""))
write.table(w2, "TitleEval\\CaseTitle-4th-1996.csv", row.names=F, col.names=T, sep=", ", quote=T)

# Identify cases with duplicated name and differing values in outcome, per curiam, pubstatus, or authors
x <- dbGetQuery(db2, "select    min(a.lni) as lni, a.courtid, c.shortname as court, a.decisiondate, d.alttitle,
                                count(distinct a.outcome) as noutcome,
                                count(distinct a.percuriam) as npercuriam,
                                count(distinct a.pubstatus) as npubstatus,
                                count(distinct b.paneltext) as npanel,
                                count(distinct b.opinionbytext) as nopinion,
                                count(distinct b.concurbytext) as nconcur,
                                count(distinct b.dissentbytext) as ndissent,
                                count(1) as ncase
                      from      CaseHeader a join CaseHeaderExt b on a.lni=b.lni
                                join Court c on a.courtid=c.id
                                join CaseAltTitle d on a.lni=d.lni
                      group  by a.courtid, c.shortname, a.decisiondate, d.AltTitle")

# Enumerate cases
sum(x[,"ncase"])

# Enumerate distinct alternate titles
nrow(x)

# Enumerate alternate titles with multiple cases (court and date), but single outcomes, per curiam, pub status, and authors
k <- which(x[,"ncase"]>1 & x[,"noutcome"]<2 & x[,"npercuriam"]<2 & x[,"npubstatus"]<2 & x[,"npanel"]<2 & x[,"nopinion"]<2 & x[,"nconcur"]<2 & x[,"ndissent"]<2)
length(k)

# Enumerate alternate titles with multiple cases and different outcome, ... values
k <- which(x[,"noutcome"]>1 | x[,"npercuriam"]>1 | x[,"npubstatus"]>1 | x[,"npanel"]>1 | x[,"nopinion"]>1 | x[,"nconcur"]>1 | x[,"ndissent"]>1)
length(k)
w <- x[k,]
rownames(w) <- NULL

# Tabulate frequency of differences by variable
table(w[,"court"])
aggregate(1:nrow(x), by=list(x[,"court"]), function(k) length(which(x[k,"noutcome"]>1))/length(k))
table(substring(w[,"decisiondate"], 1, 4))
aggregate(1:nrow(x), by=list(substring(x[,"decisiondate"], 1, 4)), function(k) length(which(x[k,"noutcome"]>1))/length(k))
table(x[,"ncase"])
table(x[,"noutcome"])
sum(table(x[which(x[,"ncase"]>3),"noutcome"]))
table(x[,"npercuriam"])
table(x[,"npubstatus"])
table(x[,"npanel"])
table(x[,"nopinion"])
table(x[,"nconcur"])
table(x[,"ndissent"])

# Render Latex table of n values by variable
# Limit to duplicated cases
y <- data.frame("n"=integer())
for(v in c("noutcome", "npanel", "nopinion", "nconcur", "ndissent", "npercuriam", "npubstatus"))
  y <- merge(y, setNames(data.frame(table(x[which(x[,"ncase"]>1),v])), c("n", v)), by="n", all=T)
for(j in 1:ncol(y))
  y[which(is.na(y[,j])),j] <- 0
y <- y[order(y[,"n"]),]
a <- paste(paste(colnames(y), collapse=" & ", sep=""), "\\\\", sep="")
for(i in 1:nrow(y))
  a <- c(a, paste(paste(format(y[i,], big.mark=","), collapse=" & ", sep=""), "\\\\", sep=""))
writeLines(a)

# Inspect cases with differences in comparison fields
j <- which(x[,"ncase"]>1 & x[,"nopinion"]>1 & x[,"nconcur"]>1 & x[,"ndissent"]>1)
k <- j[2]
y <- dbGetQuery(db2, paste("select   a.courtid, a.decisiondate, a.casetitlelexisnexis, a.outcome,
                                     b.paneltext, b.opinionbytext, b.concurbytext, b.dissentbytext
                            from     CaseHeader a join CaseHeaderExt b on a.lni=b.lni
                                     join CaseAltTitle c on a.lni=c.lni
                            where    c.alttitle='", gsub("'", "''", x[k[1],"alttitle"], fixed=T), "' and a.courtid=", x[k[1],"courtid"], " and a.decisiondate='", x[k[1],"decisiondate"], "'
                            order by a.courtid, a.decisiondate, a.casetitlelexisnexis", sep=""))
y[,"outcome"]
y[,"paneltext"]

dbGetQuery(db2, "select casetitleshort, outcome from CaseHeader where courtid=1 and decisiondate='1979-07-18' order by casetitleshort")
w <- dbGetQuery(db2, "select * from CaseHeader where courtid=1 and decisiondate='1979-09-05' order by casetitleshort")
write.table(w, "TitleEval\\CaseTitle-9th-1975-09-05.csv", row.names=F, col.names=T, sep=",", quote=T)

# Enumerate alternate titles with multiple cases, outcomes, and panels
k <- which(x[,"ncase"]>1 & x[,"noutcome"]>1 & x[,"npanel"]>1)
length(k)

# Enumerate alternate titles with multiple cases, outcomes, and opinion authors
k <- which(x[,"ncase"]>1 & x[,"noutcome"]>1 & x[,"nopinion"]>1)
length(k)

# Enumerate alternate titles with multiple cases, outcomes, and concurring authors
k <- which(x[,"ncase"]>1 & x[,"noutcome"]>1 & x[,"nconcur"]>1)
length(k)

# Enumerate alternate titles with multiple cases, outcomes, and dissenting authors
k <- which(x[,"ncase"]>1 & x[,"noutcome"]>1 & x[,"ndissent"]>1)
length(k)

# Sample short, long, and LN titles for cases with duplicated short titles
# Render in Latex
k <- which(x[,"ncase"]>2 & x[,"ncase"]<6)
length(k)
a <- ""
for(i in sample(k, 10, replace=F)) {
  w2 <- dbGetQuery(db2, paste("select a.casetitleshort, a.casetitlelong, a.casetitlelexisnexis
                               from   CaseHeader a join CaseAltTitle b on a.lni=b.lni
                               where  b.alttitle='", gsub("'", "''", x[i,"alttitle"], fixed=T), "'
                                      and a.courtid=", x[i,"courtid"], "
                                      and a.decisiondate='", x[i,"decisiondate"], "'", sep=""))
  for(j in 1:nrow(w2))
    a <- c(a,
           paste(gsub("&", "\\&", gsub("$", "\\$", w2[j,"casetitleshort"], fixed=T), fixed=T), " & ",
                 gsub("&", "\\&", gsub("$", "\\$", w2[j,"casetitlelong"], fixed=T), fixed=T), " & ",
                 gsub("&", "\\&", gsub("$", "\\$", w2[j,"casetitlelexisnexis"], fixed=T), fixed=T), " &\\\\", sep=""),
           "& & &\\\\[-4pt]")
    a <- c(a, "\\hline\\\\[-4pt]")
}
writeLines(a)

# Examine individual cases for demonstration
w <- dbGetQuery(db2, "select   a.courtid, a.decisiondate, a.casetitlelexisnexis, a.outcome,
                               b.paneltext, b.opinionbytext, b.concurbytext, b.dissentbytext
                      from     CaseHeader a join CaseHeaderExt b on a.lni=b.lni
                               join CaseAltTitle c on a.lni=c.lni
                      where    c.alttitle='guam v. ibanez' and a.decisiondate='1993-04-13'
                               or c.alttitle='norman v. lynaugh' and a.decisiondate='1988-07-05'
                               or c.alttitle='united states v. tolliver' and a.decisiondate='1995-10-19'
                               or c.alttitle like 'in re Southwest Restaurant Systems%' and a.decisiondate='1979-09-05'
                               or c.alttitle like 'Xrutherford v. bd pardon%' and a.decisiondate='2003-04-23'
                               or c.alttitle like 'r.e. serv%'
                      order by a.courtid, a.decisiondate, a.casetitlelexisnexis")

#######################################################################################
# Reproduce figure 3 using unique cases
#######################################################################################

# Identify cases with duplicated name and differing values in outcome, per curiam, pubstatus, or authors
x <- dbGetQuery(db2, "select    min(a.lni) as lni, a.courtid, c.shortname as court, a.decisiondate, d.alttitle,
                                count(distinct a.outcome) as noutcome,
                                count(distinct a.percuriam) as npercuriam,
                                count(distinct a.pubstatus) as npubstatus,
                                count(distinct b.paneltext) as npanel,
                                count(distinct b.opinionbytext) as nopinion,
                                count(distinct b.concurbytext) as nconcur,
                                count(distinct b.dissentbytext) as ndissent,
                                count(1) as ncase
                      from      CaseHeader a join CaseHeaderExt b on a.lni=b.lni
                                join Court c on a.courtid=c.id
                                join CaseAltTitle d on a.lni=d.lni
                      group  by a.courtid, c.shortname, a.decisiondate, d.AltTitle")

# Inspect cases with differences in author fields
j <- which(x[,"nopinion"]>1 & (x[,"nconcur"]>1 | x[,"ndissent"]>1))
y <- apply(as.matrix(j), 1,
       function(k) dbGetQuery(db2, 
                              paste("select   a.courtid, a.decisiondate, a.casetitlelexisnexis, a.outcome,
                                              b.paneltext, b.opinionbytext, b.concurbytext, b.dissentbytext
                                     from     CaseHeader a join CaseHeaderExt b on a.lni=b.lni
                                              join CaseAltTitle c on a.lni=c.lni
                                     where    c.alttitle='", gsub("'", "''", x[k,"alttitle"], fixed=T), "'
                                              and a.courtid=", x[k,"courtid"], " 
                                              and a.decisiondate='", x[k,"decisiondate"], "'
                                     order by a.courtid, a.decisiondate, a.casetitlelexisnexis", sep="")))

# Render Latex table containing select cases
a <- ""
for(i in 1:length(y)) {
  for(j in 1:nrow(y[[i]]))
    a <- c(a, paste("Title: & ", gsub("&", "\\&", y[[i]][j,"casetitlelexisnexis"], fixed=T), "\\\\[2pt]", sep=""),
              paste("Outcome: & ", gsub("&", "\\&", y[[i]][j,"outcome"], fixed=T), "\\\\[2pt]", sep=""),
              paste("Panel: & ", y[[i]][j,"paneltext"], "\\\\[2pt]", sep=""),
              paste("Op. by: & ", y[[i]][j,"opinionbytext"], "\\\\[2pt]", sep=""),
              paste("Conc. by: & ", y[[i]][j,"concurbytext"], "\\\\[2pt]", sep=""),
              paste("Diss. by: & ", y[[i]][j,"dissentbytext"], "\\\\[2pt]", sep=""),
              ifelse(j<nrow(y[[i]]), "\\arrayrulecolor{gray}\\hline\\\\[-4pt]", ""))
    a <- c(a, "\\arrayrulecolor{black}\\hline\\\\[-4pt]")
}
writeLines(a)

####
# Distribution of opinion, concurring, and dissenting authors by year and court
####

# Distribution of cases by author combination, both data sets
x <- dbGetQuery(db2, "select   b.ShortName as court, year(a.DecisionDate) as year,
                               concat(case when(c.o=1)then 'o' else '-' end,
                               concat(case when(c.c=1)then 'c' else '-' end,
                                      case when(c.d=1)then 'd' else '-' end)) as pattern,
                               count(1) as n
                      from     CaseHeader a join Court b on a.CourtID=b.ID
                               join ( select   min(a.lni) as lni,
                                               max(case when(OpinionType='Opinion' and char_length(JudgeID)>0)then 1 else 0 end) as o,
                                               max(case when(OpinionType='Concur' and char_length(JudgeID)>0)then 1 else 0 end) as c,
                                               max(case when(OpinionType='Dissent' and char_length(JudgeID)>0)then 1 else 0 end) as d
                                      from     CaseHeader a join Opinion b on a.lni=b.lni
                                               join CaseAltTitle c on a.lni=c.lni
                                      group by a.courtid, a.decisiondate, c.alttitle
                                    ) c on a.lni=c.lni
                      group by b.ShortName, year(a.DecisionDate),
                               concat(case when(c.o=1)then 'o' else '-' end,
                               concat(case when(c.c=1)then 'c' else '-' end,
                                      case when(c.d=1)then 'd' else '-' end))")

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

# Plot
png(paste(imgdir, "\\images\\Fig3-Dups-Omitted.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_bar(data=x, aes(x=year-0.25, y=n, fill=pattern), position="stack", stat="identity") +
  scale_fill_manual(name="o=opinion, c=concur, d=dissent",
                    values=c("ocd"="#984EA3", "oc-"="#F781BF", "o-d"="#4DAF4A", "o--"="#E41A1C",
                             "-cd"="#FF7F00", "-c-"="#FFFF33", "--d"="#A65628", "---"="#377EB8")) +
  #geom_vline(data=data.frame("x"=seq(1973.5, 2018.5, 1)), aes(xintercept=x), color="gray85", size=0.1) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
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
  labs(x="\nyear", y="cases\n")
dev.off()

# Compute average outcome length for duplicated cases
# Distribution of cases by author combination, both data sets
x <- dbGetQuery(db2, "select a.outcome
                      from   CaseHeader a join CaseAltTitle b on a.lni=b.lni
                             join ( select   a.courtid, a.decisiondate, b.alttitle
                                    from     CaseHeader a join CaseAltTitle b on a.lni=b.lni
                                    group by a.courtid, a.decisiondate, b.alttitle
                                    having   count(1)>1
                             ) c on a.courtid=c.courtid and a.decisiondate=c.decisiondate and b.alttitle=c.alttitle")

length(which(is.na(x[,"outcome"])))/nrow(x)

#######################################################################################
# Sample cases with largest difference in number of legal topics between data sets A and B
#######################################################################################

x <- dbGetQuery(db1, "select   a.lni, ifnull(t1.n, 0) as n1, ifnull(t2.n, 0) as n2,
                               c.shortname as court, a.decisiondate, a.casetitleshort
                      from     CaseHeader a 
                               left join ( select   lni, count(1) as n
                                           from     CaseLegalTopics
                                           group by lni
                                         ) t1 on a.lni=t1.lni
                               left join ( select   lni, count(1) as n
                                           from     Appeals2.CaseLegalTopics
                                           group by lni
                                         ) t2 on a.lni=t2.lni
                               join Court c on a.courtid=c.id
                      where    ifnull(t1.n, 0)<>ifnull(t2.n, 0)
                      order by abs(ifnull(t1.n, 0)-ifnull(t2.n, 0)) desc")

x[,"court"] <- gsub(" Court of Appeals", "", x[,"court"], fixed=T)

a <- ""
for(i in 1:100)
  a <- c(a,
         paste(paste(gsub("&", "\\&", x[i,"casetitleshort"], fixed=T), " & ", x[i,"court"], " & ",
                     x[i,"decisiondate"], " & ", x[i,"n1"], " & ", x[i,"n2"], sep=""),
               "\\\\", sep=""),
         "& & & &\\\\[-6pt]")
writeLines(a)

# Verify with instructions from legal topics script
x2 <- dbGetQuery(db1, "select LNI, count(1) from CaseLegalTopics group by LNI")
y2 <- dbGetQuery(db2, "select LNI, count(1) from CaseLegalTopics group by LNI")

# Merge March and Decdmber counts by case
# Retain cases that do not appear in the alternate dat set
z2 <- merge(x2, y2, by="LNI", all=T)
colnames(z2) <- c("LNI", "n1", "n2")

# Convert counts to 0 for cases missing in one data set
z2[which(is.na(z2[,"n1"])),"n1"] <- 0
z2[which(is.na(z2[,"n2"])),"n2"] <- 0

# Compute the difference in counts, between data sets, by case
z2[,"nDiff"] <- z2[,"n2"]-z2[,"n1"]

# Inspect maximum frequencies
max(z2[,"n1"])
max(z2[,"n2"])

#######################################################################################
# Sample 4th or 11th circuit cases with each author combination in "spike' period of 1990-1994
#######################################################################################

dbGetQuery(db2, "select * from Opinion limit 20")

ocd <- c("ocd", "oc-", "o-d", "o--", "-cd", "-c-", "--d", "---")
csn <- c("4th Circuit Court of Appeals", "11th Circuit Court of Appeals")[2]
x <- lapply(ocd,
       function(p) {
         authpattern <- strsplit(p, "")[[1]]
         print(authpattern)
         dbGetQuery(db2, paste("select a.decisiondate, a.casetitleshort,
                                       ifnull(opo.judgeid, '') as opo,
                                       ifnull(opc.judgeid, '') as opc,
                                       ifnull(opd.judgeid, '') as opd
                                from   CaseHeader a join Court c on a.courtid=c.id
                                       left join Opinion opo on a.lni=opo.lni and opo.opiniontype='opinion'
                                       left join Opinion opc on a.lni=opc.lni and opc.opiniontype='concur'
                                       left join Opinion opd on a.lni=opd.lni and opd.opiniontype='dissent'
                                where  year(a.decisiondate) between 1990 and 1994
                                       and c.shortname='", csn, "'
                                       and ifnull(opo.judgeid, '')", ifelse("o" %in% authpattern, "<>", "="), "''
                                       and ifnull(opc.judgeid, '')", ifelse("c" %in% authpattern, "<>", "="), "''
                                       and ifnull(opd.judgeid, '')", ifelse("d" %in% authpattern, "<>", "="), "''", sep=""))
       }) 

# Render Latex table
ns <- c(20, 20, 20, 100, 10, 10, 10, 20)
a <- ""
i0 <- 4
#i0 <- c(1, 2, 3, 8)
for(i in i0) {
  k <- sample(1:nrow(x[[i]]), ns[i], replace=F)
  k <- k[order(x[[i]][k,"decisiondate"])]
  for(j in k)
    a <- c(a, paste(ocd[i], " & ",
                    x[[i]][j,"decisiondate"], " & ",
                    gsub("&", "\\&", x[[i]][j,"casetitleshort"], fixed=T), " & ",
                    gsub("[\\~0-9]", "", gsub("urn:entity:jud-", "", x[[i]][j,"opo"], fixed=T), fixed=F), " & ",
                    gsub("[\\~0-9]", "", gsub("urn:entity:jud-", "", x[[i]][j,"opc"], fixed=T), fixed=F), " & ",
                    gsub("[\\~0-9]", "", gsub("urn:entity:jud-", "", x[[i]][j,"opd"], fixed=T), fixed=F), " & ",
                    "\\\\", sep=""),
              "& & & & & &\\\\[-4pt]")
}
writeLines(a)

# Spot check cases
i <- 11
ttl <- c("United States v. Van Dyke", "UNITED STATES v. CROCKETT", "Slattery v. Rizzo",
         "Fant v. United States Marshal Serv.", "Elmore v. Cone Mills Corp.",
         "Zady Natey, Inc. v. United Food & Commercial Workers Int''l Union, Local No. 27",
         "Shaw v. Stroud", "Republican Party v. Hunt", "UNITED STATES v. BOARD",
         "Hutchinson v. Town of Elkton",
         "United States ex rel. Barber-Colman Co. v. United States Fidelity & Guar. Co.")[i]
dt <- c("1990-02-12", "1990-11-13", "1991-07-25", "1993-10-27", "1994-05-06", "1993-06-01",
        "1994-01-06", "1993-04-27", "1991-04-05", "1990-05-24", "1994-03-21")[i]
dbGetQuery(db2, paste("select b.*
                       from   CaseHeader a join Opinion b on a.lni=b.lni
                       where  courtid=13
                              and a.casetitleshort='", ttl, "' 
                              and decisiondate='", dt, "'", sep=""))

#######################################################################################
# Dirt
#######################################################################################

z <- dbGetQuery(db2, "select decisiondate, casetitleshort, outcome from CaseHeader where casetitleshort like '%trump%' order by decisiondate")
z[which(substring(z[,"decisiondate"], 1, 4)<"2017"),"outcome"]
