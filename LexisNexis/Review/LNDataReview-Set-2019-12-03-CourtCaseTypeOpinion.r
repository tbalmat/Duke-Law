# Duke University Law Appeals Analysis
# Review of 2019-12-03 LexisNexis Data
# Court, CaseType, CaseOutcomeType, Court, Opinion, OpinionBy, ConcurBy, DissentBy, duplicate cases

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

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-03-28")
lnsourcedir1 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-03-13"
lnsourcedir2 <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisData-2019-12-03"
imgdir <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\Review\\2020-03-28"


#######################################################################################
# Connect to Appeals databases
# db1 for data set 1, March 2019
# db2 for data set 2, December 2019
#######################################################################################

#dbDisconnect(db1)
#dbDisconnect(db2)

usr <- "tjb48"
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

#######################################################################################
# Verify LNI format
#######################################################################################

# LNIs have format XXXX-XXXX-XXXX-XXXX-00000-0X, where each X is either a
# single integer (0-9) or upper case character (A-Z)

# March
x <- do.call(rbind,
       apply(as.matrix(0:10), 1,
         function(i)
           data.frame("lni"=read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                                       header=T, sep=",", quote="\"", comment="", strip.white=T)[,"lni"])))[,1]

# December
y <- do.call(rbind,
       apply(as.matrix(0:11), 1,
         function(i)
           data.frame("lni"=read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                                       header=T, sep=",", quote="\"", comment="", strip.white=T)[,"lni"])))[,1]

# Distribution of string length
table(nchar(x))
table(nchar(y))

# Unique characters by position
i <- 28
sort(unique(substring(x, i, i)))

# Identify LNIs that do not conform to the official pattern
# The following grep pattern is taken from a review of the March data
lapply(list(x, y),
  function(z)
    z[grep(paste("[34578][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                 "[0123456789BCDFGHJKMNPRSTVWXY]{3}[012]-",
                 "[026DFJKTY][0123456789BCDFGHJKMNPRSTVWXY]{3}-",
                 "[0123456789BCDFGHJKMNPRSTVWXY][0-5][0123456789BCDFGHJKMNPRSTVWXY]{2}-",
                 "00000-0[0-5]", sep=""), z, invert=T)])

# Compare numbers of source and database LNIs
length(x)
dbGetQuery(db1, "select count(distinct LNI) from CaseHeader")

# Two decisions were omitted due to extraneous text (opinion text) in the opinion-by field
# It is verified in the "Verify opinion text" section, below, that case records for related
# cases appear in the March source data with extraneous text, are omitted in the database
# containing March data, and were not included in the December source data
which(x %in% c("3T3Y-TS60-0038-X1MJ-00000-00", "3T8C-CSW0-0038-X3XN-00000-00"))
dbGetQuery(db1, "select * from CaseHeader where lni in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00')")
dbGetQuery(db2, "select * from CaseHeader where lni in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00')")

length(y)
dbGetQuery(db2, "select count(distinct LNI) from CaseHeader")

which(y %in% c("3T3Y-TS60-0038-X1MJ-00000-00", "3T8C-CSW0-0038-X3XN-00000-00"))
dbGetQuery(db2, "select * from CaseHeader where lni in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00')")

# Verify LNI uniqueness
dbGetQuery(db1, "select LNI, count(1) from CaseHeader group by LNI having count(1)>1")
z <- aggregate(1:length(x), by=list(x), length)
z[which(z[,2]>1),]

dbGetQuery(db2, "select LNI, count(1) from CaseHeader group by LNI having count(1)>1")
z <- aggregate(1:length(y), by=list(y), length)
z[which(z[,2]>1),]

# 3S4X-1450-003B-G54N-00000-00 appears 11 times in the source data
which(y=="3S4X-1450-003B-G54N-00000-00")

# All but the first record are omitted during import when LNI is repeated
dbGetQuery(db1, "select LNI from CaseHeader where LNI='3S4X-1450-003B-G54N-00000-00'")

#######################################################################################
# Verify decision dates
#######################################################################################

# Identify null dates
x <- dbGetQuery(db1, "select LNI, CourtID, DecisionDate, CaseTitleShort from CaseHeader where DecisionDate is null")
y <- dbGetQuery(db2, "select LNI, CourtID, DecisionDate, CaseTitleShort from CaseHeader where DecisionDate is null")

# Render in Latex
writeLines(paste(x[order(x[,"LNI"]),"LNI"], " & yes & no\\\\", sep=""))

# Retrieve dates from the March data
x <- do.call(rbind,
       lapply(0:10,
         function(i)
           data.frame(read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "date")])))

# Retrieve dates from the December data
y <- do.call(rbind,
       lapply(0:11,
         function(i)
           data.frame(read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "date")])))

# Evaluate date string lengths
xlni <- which(nchar(x[,"date"])!=10)
x[xlni,]
nchar(x[xlni,"date"])

ylni <- which(nchar(y[,"date"])!=10)
y[ylni,]
nchar(y[ylni,"date"])

# Evaluate date validity

# Format
xlni <- grep("[12][0-9]{3}-[01][0-9]-[0123][0-9]", x[,"date"], invert=T)
length(xlni)
x[xlni,]
ylni <- grep("[12][0-9]{3}-[01][0-9]-[0123][0-9]", y[,"date"], invert=T)
length(ylni)
y[ylni,]

# Year
xlni <- which(!substring(x[,"date"], 1, 4) %in% as.character(1973:2018))
length(xlni)
x[xlni,]
ylni <- which(!substring(y[,"date"], 1, 4) %in% as.character(1973:2018))
length(ylni)
y[ylni,]

# Month
xlni <- which(!substring(x[,"date"], 6, 7) %in% sprintf("%02.0f", 1:12))
length(xlni)
x[xlni,]
ylni <- which(!substring(y[,"date"], 6, 7) %in% sprintf("%02.0f", 1:12))
length(ylni)
y[ylni,]

# Day
xlni <- which(!substring(x[,"date"], 9, 10) %in% sprintf("%02.0f", 1:31))
length(xlni)
x[xlni,]
ylni <- which(!substring(y[,"date"], 9, 10) %in% sprintf("%02.0f", 1:31))
length(ylni)
y[ylni,]

# Day within month
yr <- substring(x[,"date"], 1, 4)
mo <- substring(x[,"date"], 6, 7)
dy <- substring(x[,"date"], 9, 10)
xlni <- which(!(mo %in% c("01", "03", "05", "07", "08", "10", "12") & dy %in% sprintf("%02.0f", 1:31) |
                mo %in% c("04", "06", "09", "11") & dy %in%  sprintf("%02.0f", 1:30) |
                !yr %in% c("1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016") &
                mo=="02" & dy %in% sprintf("%02.0f", 1:28) |
                yr %in% c("1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016") &
                mo=="02" &  dy %in% sprintf("%02.0f", 1:29)))
length(xlni)
print(x[xlni,], row.names=F)
yr <- substring(y[,"date"], 1, 4)
mo <- substring(y[,"date"], 6, 7)
dy <- substring(y[,"date"], 9, 10)
ylni <- which(!(mo %in% c("01", "03", "05", "07", "08", "10", "12") & dy %in% sprintf("%02.0f", 1:31) |
                mo %in% c("04", "06", "09", "11") & dy %in%  sprintf("%02.0f", 1:30) |
                !yr %in% c("1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016") &
                mo=="02" & dy %in% sprintf("%02.0f", 1:28) |
                yr %in% c("1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016") &
                mo=="02" &  dy %in% sprintf("%02.0f", 1:29)))
length(ylni)
y[ylni,]

# Test existence of database records with identified cases with invalid dates
db <- db2
dbGetQuery(db, paste("select lni from CaseHeader where lni in('",
                     paste(x[xlni,"lni"], collapse="', '", sep=""), "')", sep=""))

#######################################################################################
# Compare decision dates of cases appearing in both March and December 2019 data sets
#######################################################################################

# Using SQL data
x <- dbGetQuery(db1,
       "select a.lni, a.decisiondate, b.decisiondate
        from   CaseHeader a join Appeals2.CaseHeader b on a.lni=b.lni
        where  a.decisiondate!=b.decisiondate")

# Using source records
# Retrieve unique court names appearing in the March data
x <- do.call(rbind,
       lapply(0:10,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,c("lni", "date")]))
# Retrieve unique court names appearing in the December data
y <- do.call(rbind,
       lapply(0:11,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,c("lni", "date")]))
z <- merge(x, y, by="lni")
colnames(z) <- c("lni", "d1", "d2")
k <- which(z[,"d1"]!=z[,"d2"])
z[k,]
x[which(x[,"lni"]==z[k,"lni"]),]
y[which(y[,"lni"]==z[k,"lni"]),]

#######################################################################################
# Enumerate cases by year of decision
#######################################################################################

db <- db2
dbGetQuery(db, "select year(decisiondate) as year, count(1) as n from CaseHeader group by year(decisiondate)")

#######################################################################################
# Evaluate case type
#######################################################################################

db <- db1

# Cases with at least one type
dbGetQuery(db, "select CaseType, count(1) as n from CaseType group by CaseType")

# Cases with no recorded type
dbGetQuery(db, "select count(1) as n from CaseHeader where LNI not in(select LNI from CaseType)")

# Enumerate cases by category (no type, criminal, civil, criminal and civil)
x <- lapply(list(db1, db2),
       function(db)
         dbGetQuery(db, "select   a.type, count(1) as n
                         from     ( select case when(b.lni is null)then 'none'
                                                when(b.ncr>0 and b.ncv>0)then 'civ-crim'
                                                when(b.ncr>0)then 'crim'
                                                else 'civ'
                                           end as type
                                    from   CaseHeader a left join
                                           ( select   lni,
                                                      sum(case when(CaseType='criminal')then 1 else 0 end) as ncr,
                                                      sum(case when(CaseType='civil')then 1 else 0 end) as ncv
                                             from     CaseType
                                             group by lni
                                           ) b on a.lni=b.lni
                                  ) a
                         group by a.type"))
sum(x[[1]][,"n"])
sum(x[[2]][,"n"])
x[[1]][,"n"]/sum(x[,"n"])
x[[2]][,"n"]/sum(x[,"n"])

# Latex
k <- c(1, 3, 2, 4)
writeLines(paste(x[[1]][k,"type"], " & ",
                 format(x[[1]][k,"n"], big.mark=","), " (", round(x[[1]][k,"n"]/sum(x[[1]][k,"n"])*100, 1), "\\%) & ",
                 format(x[[2]][k,"n"], big.mark=","), " (", round(x[[2]][k,"n"]/sum(x[[2]][k,"n"])*100, 1), "\\%)\\\\", 
                 sep=""))

# Using source records
# March data
x <- unlist(
       lapply(0:10,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,"casetypes"]))
           
# December data
y <- unlist(
       lapply(0:11,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,"casetypes"]))

aggregate(x, by=list(x), length)
aggregate(y, by=list(y), length)

#######################################################################################
# Compare case types by case
#######################################################################################

# March data
x <- dbGetQuery(db1,
       "select a.lni, ifnull(b.ncr, 0) as ncr, ifnull(b.ncv, 0) as ncv
        from   CaseHeader a
               left join  ( select   lni,
                                     sum(case when(CaseType='criminal')then 1 else 0 end) as ncr,
                                     sum(case when(CaseType='civil')then 1 else 0 end) as ncv
                            from     CaseType
                            group by lni
                          ) b on a.lni=b.lni")

# December data
y <- dbGetQuery(db2,
       "select a.lni, ifnull(b.ncr, 0) as ncr, ifnull(b.ncv, 0) as ncv
        from   CaseHeader a
               left join  ( select   lni,
                                     sum(case when(CaseType='criminal')then 1 else 0 end) as ncr,
                                     sum(case when(CaseType='civil')then 1 else 0 end) as ncv
                            from     CaseType
                            group by lni
                          ) b on a.lni=b.lni")

# Identify differences in civil or criminal classification
z <- merge(x, y, by="lni", all=T)
colnames(z) <- c("lni", "ncr1", "ncv1", "ncr2", "ncv2")
k <- which(z[,"ncr1"]!=z[,"ncr2"] | z[,"ncv1"]!=z[,"ncv2"])
z[k,]

# Render in Latex
writeLines(paste(z[k,"lni"], " & ",
                 ifelse(z[k,"ncv1"]==0, "no", "yes"), " & ", 
                 ifelse(z[k,"ncv2"]==0, "no", "yes"), " & ",
                 ifelse(z[k,"ncr1"]==0, "no", "yes"), " & ",
                 ifelse(z[k,"ncr2"]==0, "no", "yes"), "\\\\", sep=""))
 
for(i in k)
  a <- c(a, paste(z[i,1], " & ", ifelse(z[gsub("&", "\\\\&", z[i,2]), " & ", gsub("&", "\\\\&", z[i,3]), "\\\\", sep=""))
writeLines(a)

#######################################################################################
# Enumerate cases by court
#######################################################################################

x <- dbGetQuery(db1, "select   a.ShortName, a.LongName, count(1) as n
                      from     Court a left join CaseHeader b on a.ID=b.CourtID
                      group by a.ShortName, a.LongName
                      order by a.ShortName")
y <- dbGetQuery(db2, "select   a.ShortName, a.LongName, count(1) as n
                      from     Court a left join CaseHeader b on a.ID=b.CourtID
                      group by a.ShortName, a.LongName
                      order by a.ShortName")
z <- merge(x, y, by=c("ShortName", "LongName"), all=T)
colnames(z) <- c("sn", "ln", "n1", "n2")
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0

# Latex table
a <- ""
for(i in 1:nrow(z))
a <- c(a,
       paste(ifelse(nchar(z[i,"sn"])>0, z[i,"sn"], "\\textit{empty}"), " & ",
             ifelse(nchar(z[i,"ln"])>0, z[i,"ln"], "\\textit{empty}"), " & ",
             format(z[i,"n1"], big.mark=","), " (", round(z[i,"n1"]/sum(z[,"n1"])*100, 1), "\\%) & ",
             format(z[i,"n2"], big.mark=","), " (", round(z[i,"n2"]/sum(z[,"n2"])*100, 1), "\\%)\\\\",
             sep=""),
             " & & &\\\\[-6pt]")
writeLines(a)
 
dbGetQuery(db, "select * from CaseHeader where CourtID not in(select ID from Court)")
dbGetQuery(db, "select LNI, DecisionDate, CaseTitleShort
                from   CaseHeader a join Court b on a.CourtID=b.ID
                where  b.ShortName=''")
print(sort(dbGetQuery(db, "select ShortName from Court")[,1]), row.names=F)

#######################################################################################
# List court names in each source file
#######################################################################################

# Retrieve unique court names appearing in the March data
x <- sort(unique(unlist(
       lapply(0:10,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,"courtshortname"]))))

# Retrieve unique court names appearing in the December data
y <- sort(unique(unlist(
       lapply(0:11,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")), header=T, sep=",",
                      quote="\"", comment="", strip.white=T)[,"courtshortname"]))))

#######################################################################################
# Identify cases with no court name
#######################################################################################

x <- dbGetQuery(db1,
       "select a.lni, a.courtid
        from   CaseHeader a left join Court b on a.courtid=b.id
        where  ifnull(b.shortname, '')=''")

y <- dbGetQuery(db2,
       "select a.lni, a.courtid
        from   CaseHeader a left join Court b on a.courtid=b.id
        where  ifnull(b.shortname, '')=''")

z <- merge(x, y, by="lni", all=T)
colnames(z) <- c("LNI", "setA", "setB")
z[,"setA"] <- ifelse(!is.na(z[,"setA"]), "yes", "no")
z[,"setB"] <- ifelse(!is.na(z[,"setB"]), "yes", "no")

# Render in Latex
a <- ""
for(i in 1:nrow(z))
  a <- c(a, paste(z[i,1], " & ", gsub("&", "\\\\&", z[i,2]), " & ", gsub("&", "\\\\&", z[i,3]), "\\\\", sep=""))
writeLines(a)

#######################################################################################
# Compare court names of cases appearing in both data sets
#######################################################################################

# Review court names
dbGetQuery(db1, "select ID, ShortName from Court order by ID")
dbGetQuery(db2, "select ID, ShortName from Court order by ID")

# Compare court short names by case
xc <- dbGetQuery(db1, "select a.LNI, b.ShortName as name1, d.ShortName as name2
                       from   CaseHeader a join Court b on a.CourtID=b.ID
                              join Appeals2.CaseHeader c on a.lni=c.lni
                              join Appeals2.Court d on c.CourtID=d.ID
                       where  b.ShortName!=d.ShortName")

# Verify SQL results in source records
# Retrieve court names from the March data
x <- do.call(rbind,
       lapply(0:10,
         function(i)
           data.frame(read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "courtshortname", "courtlongname")])))
# Retrieve court names from the December data
y <- do.call(rbind,
       lapply(0:11,
         function(i)
           data.frame(read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "courtshortname", "courtlongname")])))

# Compare short names in source records
z <- merge(x, y, by="lni")
colnames(z) <- c("lni", "sname1", "lname1", "sname2", "lname2")
k <- which(z[,"sname1"]!=z[,"sname2"])
v <- z[k,c("lni", "sname1", "sname2")]

w <- do.call(rbind,
       lapply(xc[,"LNI"],
         function(lni) data.frame("lni"=lni,
                                  "name1"=x[which(x[,"lni"]==lni),"courtshortname"],
                                  "name2"=y[which(y[,"lni"]==lni),"courtshortname"])))

# Render in Latex
a <- ""
for(i in 1:nrow(xc))
  a <- c(a,
         paste(xc[i,1], " & ",
               gsub("&", "\\\\&", ifelse(nchar(xc[i,2])>0, xc[i,2], "\\textit{empty}")), " & ",
               gsub("&", "\\\\&", ifelse(nchar(xc[i,3])>0, xc[i,3], "\\textit{empty}")), "\\\\",
               sep=""))
writeLines(a)

#######################################################################################
# Case frequency by court and year
#######################################################################################

x <- do.call(rbind,
       list(data.frame("set"="A",
                       dbGetQuery(db1, "select   b.ShortName as court, year(a.DecisionDate) as year, count(1) as n
                                        from     CaseHeader a join Court b on a.CourtID=b.ID
                                        group by a.CourtID, year(DecisionDate)")),
            data.frame("set"="B", 
                       dbGetQuery(db2, "select   b.ShortName as court, year(a.DecisionDate) as year, count(1) as n
                                        from     CaseHeader a join Court b on a.CourtID=b.ID
                                        group by a.CourtID, year(DecisionDate)"))))

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

png(paste(imgdir, "\\CourtCaseTypeOpinion\\images\\CourtYearFrequency.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=x, aes(x=year, y=log(n), shape=set), size=1.5, alpha=1) +
  #scale_color_manual(name="set", values=c("A"="blue3", "B"="red3")) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  scale_y_continuous(breaks=c(0, log(c(10, 100, 1000, 10000))), labels=function(x) format(exp(x), big.mark=",")) +
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

#######################################################################################
# Compare March and December 2019 case frequencies by court and year
#######################################################################################

# Merge March and December counts by court
# Retain courts/years that do not appear in the alternate dat set

x <- dbGetQuery(db1, "select   b.ShortName as court, year(a.DecisionDate) as year, count(1) as n
                      from     CaseHeader a join Court b on a.CourtID=b.ID
                      group by a.CourtID, year(DecisionDate)")

y <- dbGetQuery(db2, "select   b.ShortName as court, year(a.DecisionDate) as year, count(1) as n
                      from     CaseHeader a join Court b on a.CourtID=b.ID
                      group by a.CourtID, year(DecisionDate)")

z <- merge(x, y, by=c("court", "year"), all.x=T, all.y=T)
colnames(z) <- c("court", "year", "n1", "n2")

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
 
# Convert counts to 0 for court and year missing in one data set
z[1:100,]
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0

# Compute the difference in counts, between data sets, by court and year
z[,"nDiff"] <- z[,"n2"]-z[,"n1"]

# Inspect maximum frequencies
max(z[,"n1"])
max(z[,"n2"])
max(z[,"nDiff"])

# Identify by sign of difference for log conversion and coloration
kq <- which(z[,"nDiff"]>0)
kp <- which(z[,"nDiff"]>0)
kn <- which(z[,"nDiff"]<0)
png(paste(imgdir, "\\CourtCaseTypeOpinion\\images\\CourtYearFrequencyDiff.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=z[kp,], aes(x=year, y=log(nDiff)), size=2, alpha=0.5) +
  geom_point(data=z[kn,], aes(x=year, y=log(-nDiff)), size=2, alpha=0.5, color="red") +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  scale_y_continuous(breaks=c(0, log(c(10, 100, 1000, 10000))), labels=function(x) format(exp(x), big.mark=",")) +
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
  labs(x="\nyear", y="difference in cases\n")
dev.off()

#######################################################################################
# Compare case titles (short and long) for cases appearing in both data sets
#######################################################################################

dbGetQuery(db1, "select count(1) as n from CaseHeader a join Appeals2.CaseHeader b on a.lni=b.lni")

xs <- dbGetQuery(db1, "select a.lni, a.CaseTitleShort as t1, b.CaseTitleShort as t2
                       from   CaseHeader a join Appeals2.CaseHeader b on a.lni=b.lni
                       where  a.CaseTitleShort!=b.CaseTitleShort")
nrow(xs)

# Latex
k <- sample(1:nrow(xs), 20, replace=F)
writeLines(paste(xs[k,"lni"], " & ",
                 "A & ", gsub("\\$", "\\\\$", gsub("&", "\\\\&", xs[k,"t1"])), "\\\\ & ",
                 "B & ", gsub("\\$", "\\\\$", gsub("&", "\\\\&", xs[k,"t2"])), "\\\\[6pt]", sep=""))

xl <- dbGetQuery(db1, "select a.lni, a.CaseTitleLong as t1, b.CaseTitleLong as t2
                       from   CaseHeader a join Appeals2.CaseHeader b on a.lni=b.lni
                       where  a.CaseTitleLong!=b.CaseTitleLong")

nrow(xl)

# Latex
writeLines(paste(xl[,"lni"], " & ",
                 "A & ", gsub("#", "\\\\#", gsub("\\$", "\\\\$", gsub("&", "\\\\&", xl[,"t1"]))), "\\\\[4pt] & ",
                 "B & ", gsub("#", "\\\\#", gsub("\\$", "\\\\$", gsub("&", "\\\\&", xl[,"t2"]))), "\\\\[6pt]", sep=""))

#######################################################################################
# Review opinions
# Note that opinion by, concur by, and dissent by were included in both data sets
# Opinion, concurring, and dissenting text was not included in the March data
# December text data are stored in the CaseHeaderExt table
# Note that some source records reference opinion, concurring, and dissenting judges by
# judge ID, other records list textual judge names
#######################################################################################

dbGetQuery(db1, "select * from Opinion limit 10")
dbGetQuery(db2, "select * from Opinion limit 10")
dbGetQuery(db1, "select count(1) from Opinion")
dbGetQuery(db2, "select count(1) from Opinion")

# Retrieve opinions for cases that exist in both data sets
x <- dbGetQuery(db1,
     "select LNI, OpinionType, JudgeID
      from   Opinion
      where  LNI in(select LNI from Appeals2.CaseHeader)")
y <- dbGetQuery(db2,
     "select LNI, OpinionType, JudgeID
      from   Opinion
      where  LNI in(select LNI from Appeals.CaseHeader)")

nrow(x)
nrow(y)

# Enumerate cases with differing numbers of opinions by type
x2 <- aggregate(1:nrow(x), by=list(x[,"LNI"], x[,"OpinionType"]), length)
colnames(x2) <- c("LNI", "OpinionType", "n")
y2 <- aggregate(1:nrow(y), by=list(y[,"LNI"], y[,"OpinionType"]), length)
colnames(y2) <- c("LNI", "OpinionType", "n")
z <- merge(x2, y2, by=c("LNI", "OpinionType"), all=T)
colnames(z) <- c("LNI", "OpinionType", "n1", "n2")
# Convert NAs to 0
z[which(is.na(z[,"n1"])),"n1"] <- 0
z[which(is.na(z[,"n2"])),"n2"] <- 0
# Identify differences
length(which(z[,"n1"]-z[,"n2"]!=0))
z[429636,]
x[which(x[,"LNI"]=="3S4X-1450-003B-G54N-00000-00"),]
y[which(y[,"LNI"]=="3S4X-1450-003B-G54N-00000-00"),]

# Enumerate cases with different judges by opinion type
z <- merge(x, y, by=c("LNI", "OpinionType"), all=T)
colnames(z) <- c("LNI", "OpinionType", "Judge1", "Judge2")
# Omit "urn:entity:" in data set 2 judges, since it does not appear in data set 1
z[,"Judge2"] <- gsub("urn:entity:", "", z[,"Judge2"])
length(which(z[,"Judge1"] != z[,"Judge2"]))
z[which(z[,"Judge1"] != z[,"Judge2"]),][1:20,c("Judge1", "Judge2")]
# Convert ~~ to ~
z[,"Judge1"] <- gsub("~~", "~", z[,"Judge1"])
z[,"Judge2"] <- gsub("~~", "~", z[,"Judge2"])
# Count difference in number of judge IDs
w1 <- unlist(lapply(gregexpr("jud-", z[,"Judge1"]), length))
w2 <- unlist(lapply(gregexpr("jud-", z[,"Judge2"]), length))
length(which(w1>w2))
length(which(w1<w2))
z[which(w1>w2),][1:20,c("Judge1", "Judge2")]

#######################################################################################
# Verify that opinion text appears in opinion-by field in source data for select cases
# Case records for the LNIs reviewed below were omitted on import due to extraneous
# text appearing opinion-by field
#######################################################################################

# March data (where additional text was initially observed)
x <- lapply(c(1, 8),
       function(i) {
         x <- read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                         header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "opinionby")]
         x[which(x[,"lni"] %in% c("3T3Y-TS60-0038-X1MJ-00000-00", "3T8C-CSW0-0038-X3XN-00000-00")),]
       })

# December data
y <- lapply(0:11,
       function(i) {
         x <- read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                         header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "opinionby")]
         x[which(x[,"lni"] %in% c("3T3Y-TS60-0038-X1MJ-00000-00", "3T8C-CSW0-0038-X3XN-00000-00")),]
       })

# Verify absence of 3T3Y-TS60-0038-X1MJ-00000-00 and 3T8C-CSW0-0038-X3XN-00000-00 in both data sets
db <- db2
dbGetQuery(db, "select *
                from   Opinion
                where  lni in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00')")

#######################################################################################
# Distribution of opinion, concurring, and dissenting author combinations
# Enumerate cases with no author, opinion author only, concurring author only,
# dissenting author only, or some combination of authors
#######################################################################################

db <- db2

# Inspect opinion records
dbGetQuery(db, "select * from Opinion where OpinionType='dissent' and char_length(JudgeID)>10 limit 100")

# Distribution of opinion type
dbGetQuery(db, "select   OpinionType, sum(case when(char_length(JudgeID)>0)then 1 else 0 end) as n
                from     Opinion
                group by OpinionType")

# Cases with no opinion author
dbGetQuery(db, "select count(1)
                from   CaseHeader a join Opinion b on a.lni=b.lni
                where  b.OpinionType='Opinion' and char_length(b.JudgeID)=0")

# Cases with no opinion, concurring, or dissenting author
dbGetQuery(db, "select count(1)
                from   CaseHeader
                where  lni not in(select lni from Opinion where char_length(JudgeID)>0)")

# Opinion types
dbGetQuery(db, "select distinct OpinionType from Opinion")

# Distribution of cases by author combination, both data sets
x <- lapply(list(db1, db2),
       function(db)
         dbGetQuery(db, "select   pattern, count(1) as n
                         from     ( select   max(case when(OpinionType='Opinion' and char_length(JudgeID)>0)then 100 else 0 end) +
                                             max(case when(OpinionType='Concur' and char_length(JudgeID)>0)then 10 else 0 end) +
                                             max(case when(OpinionType='Dissent' and char_length(JudgeID)>0)then 1 else 0 end) as pattern
                                    from     Opinion
                                    group by lni
                                  ) a
                         group by pattern"))

# Combine frequencies by pattern
z <- merge(x[[1]], x[[2]], by="pattern")

# Render Latex table
k <- order(z[,1], decreasing=T)
writeLines(paste(ifelse(z[k,1]>=100, "y", "n"), " & ",
                 ifelse(as.integer(z[k,1]/10)%%2>0, "y", "n"), " & ",
                 ifelse(z[k,1]%%2>0, "y", "n"), " & ",
                 format(z[k,2], big.mark=","), " (", round(z[k,2]/sum(z[k,2])*100, 1), "\\%) & ",
                 format(z[k,3], big.mark=","), " (", round(z[k,3]/sum(z[k,3])*100, 1), "\\%)\\\\", sep=""))

#######################################################################################
# Distribution of opinion, concurring, and dissenting authors by year and court
#######################################################################################

# Distribution of cases by author combination, both data sets
x <- do.call(rbind,
       lapply(c("A", "B"),
         function(set) {
           if(set=="A") {
             db <- db1
           } else {
             db <- db2
           }
           data.frame("set"=set,
                      dbGetQuery(db, "select   b.ShortName as court, year(a.DecisionDate) as year,
                                               concat(case when(c.o=1)then 'o' else '-' end,
                                               concat(case when(c.c=1)then 'c' else '-' end,
                                                      case when(c.d=1)then 'd' else '-' end)) as pattern,
                                               count(1) as n
                                      from     CaseHeader a join Court b on a.CourtID=b.ID
                                               join ( select   lni,
                                                               max(case when(OpinionType='Opinion' and char_length(JudgeID)>0)then 1 else 0 end) as o,
                                                               max(case when(OpinionType='Concur' and char_length(JudgeID)>0)then 1 else 0 end) as c,
                                                               max(case when(OpinionType='Dissent' and char_length(JudgeID)>0)then 1 else 0 end) as d
                                                      from     Opinion
                                                      group by lni
                                                    ) c on a.lni=c.lni
                                      group by b.ShortName, year(a.DecisionDate),
                                               concat(case when(c.o=1)then 'o' else '-' end,
                                               concat(case when(c.c=1)then 'c' else '-' end,
                                                      case when(c.d=1)then 'd' else '-' end))"))
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

# Inspect
k1 <- which(x[,"set"]=="A")
k2 <- which(x[,"set"]=="B")
w <- merge(x[k1,], x[k2,], by=c("court", "year", "pattern"), all=T)

# Compare with results of previous section (SQL aggregation by pattern)
aggregate(1:nrow(w), by=list(w[,"pattern"]), function(k) c(sum(w[k,"n.x"], na.rm=T), sum(w[k,"n.y"], na.rm=T)))

# Plot
png(paste(imgdir, "\\CourtCaseTypeOpinion\\images\\OpinionTypeCourtYearFrequency.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_bar(data=x[which(x[,"set"]=="A"),], aes(x=year-0.25, y=n, fill=pattern), position="stack", stat="identity", width=0.35) +
  geom_bar(data=x[which(x[,"set"]=="B"),], aes(x=year+0.25, y=n, fill=pattern), position="stack", stat="identity", width=0.35) +
  scale_fill_manual(name="o=opinion, c=concur, d=dissent",
                    values=c("ocd"="#984EA3", "oc-"="#F781BF", "o-d"="#4DAF4A", "o--"="#E41A1C",
                             "-cd"="#FF7F00", "-c-"="#FFFF33", "--d"="#A65628", "---"="#377EB8")) +
  geom_vline(data=data.frame("x"=seq(1973.5, 2018.5, 1)), aes(xintercept=x), color="gray85", size=0.1) +
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

#######################################################################################
# Compare March and December opinion, concurring, and dissenting author types by case
#######################################################################################

# Compose author combinations by LNI in each data set
x <- lapply(list(db1, db2),
       function(db)
         dbGetQuery(db, "select lni,
                                concat(case when(o=1)then 'o' else '-' end,
                                concat(case when(c=1)then 'c' else '-' end,
                                       case when(d=1)then 'd' else '-' end)) as pattern
                         from   ( select   lni,
                                            max(case when(OpinionType='Opinion' and char_length(JudgeID)>0)then 1 else 0 end) as o,
                                            max(case when(OpinionType='Concur' and char_length(JudgeID)>0)then 1 else 0 end) as c,
                                            max(case when(OpinionType='Dissent' and char_length(JudgeID)>0)then 1 else 0 end) as d
                                   from     Opinion
                                   group by lni
                                 ) a"))

# Join cases and evaluate differences
z <- merge(x[[1]], x[[2]], by="lni")
colnames(z) <- c("lni", "pattern1", "pattern2")
nrow(z)
k <- which(z[,"pattern1"]!=z[,"pattern2"])
length(k)
z[k,]

# Inspect cases with differences
db <- db2
dbGetQuery(db, paste("select * from CaseHeader where lni in('", paste(z[k,"lni"], collapse="', '", sep=""), "')", sep=""))
dbGetQuery(db, "select id, shortname from Court")

# Retrieve case data for March
lapply(0:10,
  function(i) {
    x <- data.frame(read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                    header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "opinionby", "concurby", "dissentby")])
    x[which(x[,"lni"] %in% z[k,"lni"]),]
  })
# Retrieve case data for December
lapply(0:11,
  function(i) {
    x <- data.frame(read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                    header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "opinionby", "concurby", "dissentby")])
    x[which(x[,"lni"] %in% z[k,"lni"]),]
  })

#######################################################################################
# Compare opinion, concurring, and dissenting judge entries by case
#######################################################################################

# Retrieve case data for March
x <- do.call(rbind,
       lapply(0:10,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "opinionby", "concurby", "dissentby")]))
# Retrieve case data for December
y <- do.call(rbind,
       lapply(0:11,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "opinionby", "concurby", "dissentby")]))

# Join data sets on lni
z <- merge(x, y, by="lni")
colnames(z) <- c("lni", "o1", "c1", "d1", "o2", "c2", "d2")

# Strip urn:entity: from December authors
z[,"o2"] <- gsub("urn:entity:", "", z[,"o2"])
z[,"c2"] <- gsub("urn:entity:", "", z[,"c2"])
z[,"d2"] <- gsub("urn:entity:", "", z[,"d2"])

# Collapse ~~ to ~
z[,"o1"] <- gsub("~~", "~", z[,"o1"])
z[,"c1"] <- gsub("~~", "~", z[,"c1"])
z[,"d1"] <- gsub("~~", "~", z[,"d1"])
z[,"o2"] <- gsub("~~", "~", z[,"o2"])
z[,"c2"] <- gsub("~~", "~", z[,"c2"])
z[,"d2"] <- gsub("~~", "~", z[,"d2"])

# Identify cases with different authors
k <- which(z["o1"]!=z[,"o2"] | z["c1"]!=z[,"c2"] | z["d1"]!=z[,"d2"]) 
length(k)

# Identify cases with different opinion authors
k <- which(z["o1"]!=z[,"o2"]) 
a <- ""
for(i in k[1:10])
  a <- c(a, paste("Opinion & ", paste(z[i,c("lni", "o1", "o2")], collapse=" & ", sep=""), "\\\\", sep=""),
         "& & &\\\\[-6pt]")
writeLines(a)

# Identify cases with different concurring authors
k <- which(z["c1"]!=z[,"c2"]) 
a <- ""
for(i in k[1:10])
  a <- c(a, paste("Concurring & ", paste(z[i,c("lni", "c1", "c2")], collapse=" & ", sep=""), "\\\\", sep=""),
         " & & &\\\\[-6pt]")
writeLines(a)

# Identify cases with different opinion authors
k <- which(z["d1"]!=z[,"d2"]) 
a <- ""
for(i in k[1:10])
  a <- c(a, paste("Dissenting & ", paste(z[i,c("lni", "d1", "d2")], collapse=" & ", sep=""), "\\\\", sep=""),
         "& & &\\\\[-6pt]")
writeLines(a)

#######################################################################################
# Evaluate case outcome type
# Note that outcome type is assigned by the SQL import algorithm using the following
# hierarchical logic (the first rule that is satisfied assigns type):
# If outcome contains "reversed" then outcome type = reversed 
# Else if outcome contains "vacated" then outcome type = vacated
# Else if outcome contains "affirmed" then outcome type = affirmed
# Else if outcome contains "denied" then outcome type = denied
# Else outcome type = other
#######################################################################################

# All cases are assigned an outcome type
# Enumerate cases by type
x <- dbGetQuery(db1, "select OutcomeType, count(1) as n from CaseOutcomeType group by OutcomeType")
y <- dbGetQuery(db2, "select OutcomeType, count(1) as n from CaseOutcomeType group by OutcomeType")
z <- merge(x, y, by="OutcomeType", all=T)
colnames(z) <- c("t", "n1", "n2")

# Latex table
k <- c(1, 2, 4, 5, 3)
writeLines(paste(z[k,"t"], " & ",
                 format(z[k,"n1"], big.mark=","), " (", round(z[k,"n1"]/sum(z[k,"n1"])*100, 1), "\\%) & ",
                 format(z[k,"n2"], big.mark=","), " (", round(z[k,"n2"]/sum(z[k,"n2"])*100, 1), "\\%)\\\\",
                 sep=""))

#######################################################################################
# Identify cases with different outcome types between data sets
#######################################################################################

x <- dbGetQuery(db1,
       "select a.lni, a.OutcomeType as outcome1, b.OutcomeType as outcome2
        from   CaseOutcomeType a join Appeals2.CaseOutcomeType b on a.lni=b.lni
        where  a.OutcomeType!=b.OutcomeType")

for(i in k)
  a <- c(a, paste(z[i,1], " & ", ifelse(z[gsub("&", "\\\\&", z[i,2]), " & ", gsub("&", "\\\\&", z[i,3]), "\\\\", sep=""))
writeLines(a)

#######################################################################################
# Enumerate cases with empty outcome text
#######################################################################################

db <- db1
dbGetQuery(db, "select count(1) as n from CaseHeader where char_length(ifnull(outcome, ''))=0")
dbGetQuery(db, "select char_length(outcome) as len, count(1) as n from CaseHeader group by char_length(outcome)")

#######################################################################################
# Sample non-empty outcome text with outcome type of other
#######################################################################################

db <- db2
dbGetQuery(db, "select a.lni, a.outcome
                from   CaseHeader a join CaseOutcomeType b on a.lni=b.lni
                where  a.outcome is not null and b.outcometype='other'
                limit  20")

# Enumerate cases with "Other" outcome type and "dismiss" in outcome text
dbGetQuery(db, "select count(1)
                from   CaseHeader a join CaseOutcomeType b on a.lni=b.lni
                where  a.outcome like '%dismiss%' and b.outcometype='other'")

# Sample other outcome types with non-empty outcome text not containing "dismiss"
x <- dbGetQuery(db, "select a.lni, a.outcome
                     from   CaseHeader a join CaseOutcomeType b on a.lni=b.lni
                     where  a.outcome is not null and a.outcome not like '%dismiss%' and b.outcometype='other'")

set <- "B"
a <- ""
for(i in sample(1:nrow(x), 20, replace=F))
  a <- c(a, paste(set, " & ", x[i,"lni"], " & ", x[i,"outcome"], "\\\\", sep=""), "& &\\\\[-4pt]")
writeLines(a)

#######################################################################################
# Compare March and December 2019 case frequencies by case outcome type and year
#######################################################################################

x <- do.call(rbind,
       list(data.frame("set"="A",
                       dbGetQuery(db1, "select   b.OutcomeType, year(a.DecisionDate) as year, count(1) as n
                                        from     CaseHeader a join CaseOutcomeType b on a.LNI=b.LNI
                                        group by b.OutcomeType, year(a.DecisionDate)")),
            data.frame("set"="B", 
                       dbGetQuery(db2, "select   b.OutcomeType, year(a.DecisionDate) as year, count(1) as n
                                        from     CaseHeader a join CaseOutcomeType b on a.LNI=b.LNI
                                        group by b.OutcomeType, year(DecisionDate)"))))

png(paste(imgdir, "\\CourtCaseTypeOpinion\\images\\OutcomeTypeYearFrequency.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=x, aes(x=year, y=n, shape=set), size=1.5, alpha=1) +
  #scale_color_manual(name="set", values=c("A"="blue3", "B"="red3")) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  #scale_y_continuous(breaks=c(0, log(c(10, 100, 1000, 10000))), labels=function(x) format(exp(x), big.mark=",")) +
  facet_wrap(~OutcomeType) +
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

#######################################################################################
# Compare outcomes in March and December data sets by case
#######################################################################################

x <- dbGetQuery(db1, " select   a.CourtID, year(a.DecisionDate) as year,
                                count(1) as n,
                                sum(case when(a.Outcome<>b.Outcome)then 1 else 0 end) as nneq
                       from     CaseHeader a join Appeals2.CaseHeader b on a.LNI=b.LNI
                       group by a.CourtID, year(a.DecisionDate)")
sum(x[,"n"])

# Verify using source records
# Retrieve March data
x <- do.call(rbind,
       lapply(0:10,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "outcome")]))
# Retrieve case data for December
y <- do.call(rbind,
       lapply(0:11,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "outcome")]))

# Join by LNI
z <- merge(x, y, by="lni")
colnames(z) <- c("lni", "o1", "o2")
k <- which(z[,"o1"]!=z[,"o2"])
z[k,]
nrow(z)

db <- db1
dbGetQuery(db, paste("select * from CaseHeader where lni in('",  paste(z[k,"lni"], collapse="', ", sep=""), "')", sep=""))
dbGetQuery(db, "select * from Court")

#######################################################################################
# Compare publication status in March and December data sets by case
#######################################################################################

db <- db1
x <- dbGetQuery(db, "select pubstatus, count(1) as n from CaseHeader group by pubstatus")
x[,"n"]/sum(x[,"n"])

# Identify cases with a difference in pub status
x <- dbGetQuery(db1, "select   year(a.decisiondate) as year,
                               sum(case when(a.pubstatus='reported')then 1 else 0 end) as nr,
                               sum(case when(a.pubstatus='unreported')then 1 else 0 end) as nu
                      from     CaseHeader a join Appeals2.CaseHeader b on a.lni=b.lni
                      where    a.pubstatus<>b.pubstatus
                      group by year(a.decisiondate)")

# Latex table
writeLines(paste(x[,"year"], " & ", x[,"nr"], " & ", x[,"nu"], "\\\\", sep=""))

#######################################################################################
# Plot proportion reported cases by court, year, and data set
#######################################################################################

# Compute proportion reported status by year and set
x <- do.call(rbind,
       lapply(c("A", "B"),
         function(set) {
           if(set=="A") {
             db <- db1
           } else {
             db <- db2
           }
           data.frame("set"=set,
                      dbGetQuery(db, "select   b.shortname as court, year(a.decisiondate) as year,
                                               sum(case when(a.pubstatus='reported')then 1 else 0 end)*1./count(1) as p
                                      from     CaseHeader a join Court b on a.courtid=b.id
                                      group by b.shortname, year(a.decisiondate)"))
         }))

# Abbreviate court names
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

png(paste(imgdir, "\\CourtCaseTypeOpinion\\images\\PubStatCourtYearFrequency.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=x, aes(x=year, y=p, shape=set), size=1.5, alpha=1) +
  #scale_color_manual(name="set", values=c("A"="blue3", "B"="red3")) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  #scale_y_continuous(breaks=c(0, log(c(10, 100, 1000, 10000))), labels=function(x) format(exp(x), big.mark=",")) +
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
  labs(x="\nyear", y="proprtion reported cases\n")
dev.off()

#######################################################################################
# Compare per curiam in March and December data sets by case
#######################################################################################

db <- db2
x <- dbGetQuery(db, "select   case when(percuriam=1)then 1 else 0 end, count(1) as n
                     from     CaseHeader
                     group by case when(percuriam=1)then 1 else 0 end")
x[,"n"]/sum(x[,"n"])

# Identify cases with a difference in per curiam indicator
x <- dbGetQuery(db1, "select   year(a.decisiondate) as year,
                               sum(case when(a.percuriam=1)then 1 else 0 end) as n1,
                               sum(case when(a.percuriam=0)then 1 else 0 end) as n0
                      from     CaseHeader a join Appeals2.CaseHeader b on a.lni=b.lni
                      where    a.percuriam<>b.percuriam
                      group by year(a.decisiondate)")

# Latex table
writeLines(paste(x[,"year"], " & ", x[,"n1"], " & ", x[,"n0"], "\\\\", sep=""))

# Verify absence of change in per curiam using source data
# Retrieve March data
x <- do.call(rbind,
       lapply(0:10,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir1, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "percuriam")]))
# Retrieve case data for December
y <- do.call(rbind,
       lapply(0:11,
         function(i)
           read.table(gzfile(sprintf("%s\\%s%02.0f%s", lnsourcedir2, "DukePart", i, ".csv.gz")),
                      header=T, sep=",", quote="\"", comment="", strip.white=T)[,c("lni", "percuriam")]))

# Inspect
aggregate(1:nrow(x), by=list(x[,"percuriam"]), length)
aggregate(1:nrow(y), by=list(y[,"percuriam"]), length)

# Join by LNI
z <- merge(x, y, by="lni")
colnames(z) <- c("lni", "pc1", "pc2")
k <- which(z[,"pc1"]!=z[,"pc2"])
length(k)
z[k,]
nrow(z)

#######################################################################################
# Plot proportion per curiam cases by court, year, and data set
#######################################################################################

# Compute proportion reported status by year and set
x <- do.call(rbind,
       lapply(c("A", "B"),
         function(set) {
           if(set=="A") {
             db <- db1
           } else {
             db <- db2
           }
           data.frame("set"=set,
                      dbGetQuery(db, "select   b.shortname as court, year(a.decisiondate) as year,
                                               sum(case when(percuriam=1)then 1 else 0 end)*1./count(1) as p
                                      from     CaseHeader a join Court b on a.courtid=b.id
                                      group by b.shortname, year(a.decisiondate)"))
         }))

# Abbreviate court names
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

png(paste(imgdir, "\\CourtCaseTypeOpinion\\images\\PerCuriamCourtYearFrequency.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_point(data=x, aes(x=year, y=p, shape=set), size=1.5, alpha=1) +
  #scale_color_manual(name="set", values=c("A"="blue3", "B"="red3")) +
  scale_shape_manual(name="set", values=c("A"=4, "B"=1)) +
  scale_x_continuous(breaks=seq(1974, 2018, 4)) +
  #scale_y_continuous(breaks=c(0, log(c(10, 100, 1000, 10000))), labels=function(x) format(exp(x), big.mark=",")) +
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
  labs(x="\nyear", y="proprtion per curiam cases\n")
dev.off()
