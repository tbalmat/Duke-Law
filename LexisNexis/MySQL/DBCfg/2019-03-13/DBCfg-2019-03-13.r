# Duke University Law School Appeals Project
# R script to create MySQL database from data supplied by LexisNexis in March 2019
# Note that the following SQL commands are taken from the DBCfg.sql file and are implemented
# here for ease of scripting
# Tom Balmat

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)

library(DBI)
library(RMySQL)

############################################################################################################################
#### (Re)create case header, court, type, panel, and topics tables
############################################################################################################################

createTables <- function() {
  dbGetQuery(db, "drop table if exists CaseHeader;")
  dbGetQuery(db, "create table CaseHeader(
    ID int not null auto_increment primary key,
    LNI varchar(50) not null,
    DecisionDate date null,
    CourtID smallint not null,
    CaseTitleShort varchar(500) not null,
    CaseTitleLong varchar(2000) not null,
    CaseTitleLexisNexis varchar(500) not null,
    PubStatus varchar(25) not null,
    PerCuriam bit not null default 0,
    Outcome varchar(2000) null);")
  dbGetQuery(db, "drop table if exists Court;")
  dbGetQuery(db, "create table Court(
    ID smallint not null auto_increment primary key,
    ShortName varchar(50) not null,
    LongName varchar(100) not null);")
  dbGetQuery(db, "drop table if exists CaseType;")
  dbGetQuery(db, "create table CaseType(
    LNI varchar(50) not null,
    CaseType varchar(25) not null,
    primary key(LNI, CaseType));")
  dbGetQuery(db, "drop table if exists Judge;")
  dbGetQuery(db, "create table Judge(
    ID varchar(25) not null primary key,
    Name varchar(100) null);")
  dbGetQuery(db, "drop table if exists Panel;")
  dbGetQuery(db, "create table Panel(
    LNI varchar(50) not null,
    JudgeID varchar(25) not null,
    primary key(LNI, JudgeID));")
  dbGetQuery(db, "drop table if exists Opinion;")
  dbGetQuery(db, "create table Opinion(
    LNI varchar(50) not null,
    OpinionType varchar(25) not null,
    JudgeID varchar(255) not null,
    primary key(LNI, OpinionType, JudgeID));")
  dbGetQuery(db, "drop table if exists CaseLegalTopics;")
  dbGetQuery(db, "create table CaseLegalTopics(
    LNI varchar(50) not null,
    TopicID varchar(50) not null,
    primary key(LNI, TopicID));")
  dbGetQuery(db, "drop table if exists CaseLNTopics;")
  dbGetQuery(db, "create table CaseLNTopics(
    LNI varchar(50) not null,
    TopicID varchar(25) not null,
    primary key(LNI, TopicID));")
}

createkIndices <- function() {
  # Generate rows of consecutive integers from 1 to 65,536
  # These are used in delimited text parsing operations
  dbGetQuery(db, "drop table if exists k;")
  dbGetQuery(db, "create table k(k int auto_increment primary key, x tinyint);")
  dbGetQuery(db, "
   insert into k(x) select a.k
    from   (select 1 as k union all select 1 union all select 1 union all select 1) a
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) b
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) c
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) d
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) e
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) f
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) g
           cross join (select 1 as k union all select 1 union all select 1 union all select 1) h;")
}

createImportRec <- function() {
  dbGetQuery(db, "drop table if exists ImportRec;")
  dbGetQuery(db, "
    create table ImportRec(
    ID int not null auto_increment primary key,
    LNI varchar(50) not null,
    DecisionDate varchar(20) null,
    DateValid bit default 0,
    CourtShortName varchar(50) not null,
    CourtLongName varchar(100) not null,
    CaseTitleShort varchar(500) not null,
    CaseTitleLong varchar(2000) not null,
    CaseTitleLexisNexis varchar(500) not null,
    CaseType varchar(100) not null,
    PubStatus varchar(25) not null,
    Panel varchar(500) not null,
    OpinionBy varchar(750) not null,
    ConcurBy varchar(500) not null,
    DissentBy varchar(500) not null,
    PerCuriam varchar(25) null,
    Outcome varchar(1000) null,
    LegalTopics varchar(20000) null,
    LNTopics varchar(10000) null);")
}

loadData <- function(n) {
  # The load data command does not permit use of a variable (that could be modified using concat() in na proc) as the infile
  # Therefore, modify the 'LNAppeals00.csv' portion of the following load data command and execute for each file to be imported
  # Be careful to observe the reported number of records written, skipped, and errors (use show warnings;)
  dbGetQuery(db, paste(
    "load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/LNAppeals", sprintf("%02d", n), ".csv'
     into table ImportRec fields terminated by ',' optionally enclosed by '\"' lines terminated by '\n' ignore 1 rows
     (@col1, @col2, @col3, @col4, @col5, @col6, @col7, @col8, @col9, @col10, @col11, @col12, @col13, @col14, @col15, @col16, @col17, @col18, @col19, @col20)
     set LNI=@col1, DecisionDate=trim(@col2), CourtShortName=@col3, CourtLongName=@col4, CaseTitleShort=@col6,
         CaseTitleLong=left(@col7, 2000), CaseTitleLexisNexis=@col5, CaseType=@col9, PubStatus=@col10, Panel=@col11,
         OpinionBy=@col13, ConcurBy=@col14, DissentBy=@col15, PerCuriam=@col16, Outcome=@col17, LegalTopics=@col18, LNTopics=@col20;", sep=""))
}

createCourtRecords <- function() {
  dbGetQuery(db, "
    insert into Court(ShortName, LongName)
    select distinct trim(CourtShortName), trim(CourtLongName)
    from   ImportRec i left join Court c on i.CourtShortName=c.ShortName and i.CourtLongName=c.LongName
    where  c.ID is null;")
}

createCaseRecords <- function() {
  dbGetQuery(db, "
    insert into CaseHeader(LNI, DecisionDate, CourtID, CaseTitleShort, CaseTitleLong, CaseTitleLexisNexis,
                           PubStatus, PerCuriam, Outcome)
    select i.LNI, case when(character_length(replace(i.DecisionDate, ' ', ''))>0)then i.DecisionDate else NULL end,
           c.ID, left(trim(i.CaseTitleShort), 500), left(trim(i.CaseTitleLong), 2000),
           left(trim(i.CaseTitleLexisNexis), 500), trim(i.PubStatus),
           case when(character_length(replace(i.PerCuriam, ' ', ''))>0)then 1 else 0 end,
           case when(character_length(trim(i.Outcome))>0)then left(trim(i.Outcome), 2000) else null end
    from   ImportRec i join Court c on trim(i.CourtShortName)=c.ShortName and trim(i.CourtLongName)=c.LongName;")
}

createCaseTypeRecords <- function() {
  # Parse case type and save individual values in records with reference to case ID
  # Given n case types, generate n copies by joinining to integer (k) records
  # Use substring_index to extract text preceding kth delimiter (inner call using k) then
  # use substring_index again to extract text trailing last delimiter in inner text (outer call with parameter -1)
  # Note that the index parameter of substring instructs to return all text prior to the kth delimiter (from the left)
  # when k is positive and all text following the kth delimiter (from the right) when k is negative
  # Note, also, the use of length(text with delimiters)-length(text without delimiters) to compute number
  # of delimiters (which is one less than the required number of text record copies) 
  dbGetQuery(db, "
    insert into CaseType(LNI, CaseType)
    select i.LNI, trim(substring_index(substring_index(i.CaseType, '|', k.k), '|', -1))
    from   ImportRec i join k on k<=character_length(i.CaseType)-character_length(replace(i.CaseType, '|', ''))+1
    where  character_length(i.CaseType)>0;")
}

createPanelRecords <- function() {
  dbGetQuery(db, "
    insert into Panel(LNI, JudgeID)
    select i.LNI, trim(substring_index(substring_index(i.Panel, '|', k.k), '|', -1))
    from   ImportRec i join k on k<=character_length(i.Panel)-character_length(replace(i.Panel, '|', ''))+1
    where  character_length(i.Panel)>0;")
}

createLegalTopicsRecords <- function() {
  # Duplicates exist, so save uniques
  dbGetQuery(db, "
    insert into CaseLegalTopics(LNI, TopicID)
    select distinct i.LNI, trim(substring_index(substring_index(i.LegalTopics, '|', k.k), '|', -1))
    from   ImportRec i join k on k<=character_length(i.LegalTopics)-character_length(replace(i.LegalTopics, '|', ''))+1
    where  character_length(i.LegalTopics)>0;")
}

createLNTopicsRecords <- function() {
  dbGetQuery(db, "
    insert into CaseLNTopics(LNI, TopicID)
    select distinct i.LNI, trim(substring_index(substring_index(i.LNTopics, '|', k.k), '|', -1))
    from   ImportRec i join k on k<=character_length(i.LNTopics)-character_length(replace(i.LNTopics, '|', ''))+1
    where  character_length(i.LNTopics)>0;")
}

createOpinionRecords <- function() {
  dbGetQuery(db, "
    insert into Opinion(LNI, OpinionType, JudgeID)
    select LNI, 'Opinion', OpinionBy
    from   ImportRec;")
  dbGetQuery(db, "
    insert into Opinion(LNI, OpinionType, JudgeID)
    select LNI, 'Concur', ConcurBy
    from   ImportRec;")
  dbGetQuery(db, "
    insert into Opinion(LNI, OpinionType, JudgeID)
    select LNI, 'Dissent', DissentBy
    from   ImportRec;")
}

#################################################################################################################################################
#### Execution begins here
#### Calls to dbGetQuery return a (0, 0) dimension data frame on successful completion
#### Otherwise, a data frame containing a MySQL error message
#################################################################################################################################################

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

db <- dbConnect(MySQL(), host='localhost', user='tjb48', password='', dbname='Appeals')
createTables()
createkIndices()

i <- 0
t0 <- proc.time()[3]
while(i<11) {

  d <- createImportRec()
  if(nrow(d)!=0)
    stop(paste("Error in createImportRec:\n", d[1,1], sep=""))
  cat(i, "createImportRec\n")

  d <- loadData(i)
  if(nrow(d)!=0)
    stop(paste("Error in loadData:\n", d[1,1], sep=""))
  cat(i, "loadData\n")

  # Omit known erroneous records
  d <- dbGetQuery(db, "delete from ImportRec where LNI in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00');")
  if(nrow(d)!=0)
    stop(paste("Error in delete erroneous recs:\n", d[1,1], sep=""))
  cat(i, "del erroneous\n")

  d <- createCourtRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create court recs:\n", d[1,1], sep=""))
  cat(i, "create court recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createCaseRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create case recs:\n", d[1,1], sep=""))
  cat(i, "create case recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createCaseTypeRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create case type recs:\n", d[1,1], sep=""))
  cat(i, "create case type recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createPanelRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create panel recs:\n", d[1,1], sep=""))
  cat(i, "create panel recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createLegalTopicsRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create legal topics recs:\n", d[1,1], sep=""))
  cat(i, "create legal topics recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createLNTopicsRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create LN topics recs:\n", d[1,1], sep=""))
  cat(i, "create LN topics recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  i <- i+1

}

# Independent load of opinions
# Note that some source records reference opinion, concurring, and dissenting judges by
# judge ID, other records list textual judge names
i <- 0
t0 <- proc.time()[3]
while(i<11) {

  d <- createImportRec()
  if(nrow(d)!=0)
    stop(paste("Error in createImportRec:\n", d[1,1], sep=""))
  cat(i, "createImportRec\n")

  d <- loadData(i)
  if(nrow(d)!=0)
    stop(paste("Error in loadData:\n", d[1,1], sep=""))
  cat(i, "loadData\n")

  # Omit known erroneous records
  d <- dbGetQuery(db, "delete from ImportRec where LNI in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00');")
  if(nrow(d)!=0)
    stop(paste("Error in delete erroneous recs:\n", d[1,1], sep=""))
  cat(i, "del erroneous\n")

  d <- createOpinionRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create opinion recs:\n", d[1,1], sep=""))
  cat(i, "create opinion recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  i <- i+1

}

# Create indexes and foreign keys
dbGetQuery(db, "create unique index CaseHeaderLNI on CaseHeader(LNI);")
dbGetQuery(db, "create index CaseHeaderDecisionDate on CaseHeader(Class);")
dbGetQuery(db, "create index CaseHeaderCourtID on CaseHeader(CourtID);")
dbGetQuery(db, "create index CaseHeaderCaseTitleShort on CaseHeader(CaseTitleShort);")
dbGetQuery(db, "create index CaseHeaderPubStatus on CaseHeader(PubStatus);")
dbGetQuery(db, "create index CaseHeaderPerCuriam on CaseHeader(PerCuriam);")
dbGetQuery(db, "create unique index CourtShortName on Court(ShortName);")
dbGetQuery(db, "create unique index CourtLongName on Court(LongName);")
dbGetQuery(db, "create unique index CaseTypeLNICaseType on CaseType(LNI, CaseType);")
dbGetQuery(db, "create index CaseTypeCaseType on CaseType(CaseType);")
dbGetQuery(db, "create index PanelJudgeID on Panel(JudgeID);")
dbGetQuery(db, "create index INOpinionType on Opinion(OpinionType);")
dbGetQuery(db, "create index INOpinionJudgeID on Opinion(JudgeID);")
dbGetQuery(db, "create index INCaseLegalTopicsID on CaseLegalTopics(TopicID);")
dbGetQuery(db, "create index INLegalTopicsDescription on LegalTopics(Description);")
dbGetQuery(db, "create index INCaseLNTopicsID on CaseLNTopics(TopicID);")
dbGetQuery(db, "create index INLNTopicsDescription on LNTopics(Description);")

dbGetQuery(db, "alter table CaseHeader add constraint FKCaseHeaderCourt foreign key (CourtID) references Court(ID);")
dbGetQuery(db, "alter table CaseType add constraint FKCaseTypeLNI foreign key (LNI) references CaseHeader(LNI);")
dbGetQuery(db, "alter table Panel add constraint FKPanelLNI foreign key (LNI) references CaseHeader(LNI);")
dbGetQuery(db, "alter table Panel add constraint FKPanelJudgeID foreign key (JudgeID) references Judge(ID);")
dbGetQuery(db, "alter table Opinion add constraint FKOpinionLNI foreign key (LNI) references CaseHeader(LNI);")
dbGetQuery(db, "alter table Opinion add constraint FKOpinionJudgeID foreign key (JudgeID) references Judge(ID);")
dbGetQuery(db, "alter table Opinion add constraint CKOpinionType check(OpinionType in('Opinion', 'Concur', 'Dissent'));")
dbGetQuery(db, "alter table CaseLegalTopics add constraint FKCaseLegalTopicsLNI foreign key (LNI) references CaseHeader(LNI);")
dbGetQuery(db, "alter table CaseLegalTopics add constraint FKCaseLegalTopicsTopicID foreign key (TopicID) references LegalTopics(ID);")
dbGetQuery(db, "alter table CaseLNTopics add constraint FKCaseLNTopicsLNI foreign key (LNI) references CaseHeader(LNI);")
dbGetQuery(db, "alter table CaseLNTopics add constraint FKCaseLNTopicsTopicID foreign key (TopicID) references LNTopics(ID);")
dbGetQuery(db, "alter table Citation add constraint FKCitationLNI foreign key (LNI) references CaseHeader(LNI);")
dbGetQuery(db, "alter table Citation add constraint FKCitationLNICited foreign key (LNICited) references CaseHeader(LNI);")


####################################################################################################
#### Disconnect
####################################################################################################

dbDisconnect(db)
