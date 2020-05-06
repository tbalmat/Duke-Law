# Duke University Law School Appeals Project

# R script to create MySQL database from data supplied by LexisNexis in December 2019
# Note that the following SQL commands are adapted from the DBCfg.sql file and are implemented
# here for ease of scripting

# Due to differences in behavior when executing load data local infile from various clients, it is
# recommended that this script be executed from within a terminal session connected to the database server

# Notes on load data local infile:
# 1. The MySQL global variable local_infile must be set to 1
#    Use:
#    select @@GLOBAL.local_infile;
#    set global local_infile=1;
# 2. Functions in R and MySQL Workbench on Windows (with possible addition of secure-file-priv parameter of
#    C:\ProgramData\MySQL\MySQL Server 8.0\my.ini)
# 3. Functions with MariaDB (Red Hat) from R and MySQL client in a ssh terminal session, requiring no
#    modification to /etc/mysql/conf.d
# 4. Does not function with MariaDB (Red Hat) from R and MySQL Workbench making connections using port
#    3306 forwarding (the same terminal session from which load dat is successful), regardless of
#    modifications to /etc/mysql/conf.d (local-infile, loose-local-infile, etc.)

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
    -- ID int not null auto_increment primary key,
    LNI varchar(50) not null primary key,
    DecisionDate date null,
    CourtID smallint not null,
    CaseTitleShort varchar(500) not null,
    CaseTitleLong varchar(2000) not null,
    CaseTitleLexisNexis varchar(500) not null,
    PubStatus varchar(25) not null,
    PerCuriam bit not null default 0,
    Outcome varchar(2000) null);")
  dbGetQuery(db, "drop table if exists CaseHeaderExt;")
  dbGetQuery(db, "create table CaseHeaderExt(
    LNI varchar(50) primary key,
    PanelText varchar(5000) null,
    OpinionByText varchar(2000) null,
    ConcurByText varchar(1000) null,
    DissentByText varchar(1000) null,
    CoreTerms varchar(1000) null);")
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
  dbGetQuery(db, "drop table if exists CaseOutcomeType;")
  dbGetQuery(db, "create table CaseOutcomeType(
    LNI varchar(50) not null,
    OutcomeType varchar(25) not null,
    primary key(LNI, OutcomeType));")
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
  dbGetQuery(db, "drop table if exists LegalTopics;")
  dbGetQuery(db, "create table LegalTopics(
    ID varchar(50) not null primary key,
    Hierarchy varchar(1000) not null,
    Note varchar(1000) not null,
    Status varchar(10) not null);")
  dbGetQuery(db, "drop table if exists ShepardTreatment;")
  dbGetQuery(db, "create table ShepardTreatment(
    ID smallint not null auto_increment primary key,
    Letter varchar(10) not null,
    CurrentCategory varchar(50) not null,
    SubCategory varchar(50) not null,
    CaseStatute varchar(25) not null,
    TreatmentPhrase varchar(100) not null,
    Description varchar(1000) not null,
    UsageNotes varchar(1000) not null,
    ShepardSignal varchar(100) not null);")
  dbGetQuery(db, "drop table if exists Citation;")
  dbGetQuery(db, "create table Citation(
    ID int not null auto_increment primary key,
    LNI varchar(50) not null,
    LNICited varchar(50) not null,
    NormCitation varchar(100) not null,
    PrimaryReporter bit not null default 0,
    ShepardTreatmentID varchar(10) not null);")
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
    PanelText varchar(5000) null,
    OpinionBy varchar(750) not null,
    OpinionByText varchar(2000) null,
    ConcurBy varchar(500) not null,
    ConcurByText varchar(1000) null,
    DissentBy varchar(500) not null,
    DissentByText varchar(1000) null,
    PerCuriam varchar(25) null,
    Outcome varchar(1000) null,
    LegalTopics varchar(20000) null,
    CoreTerms varchar(1000) null);")
}

loadData <- function(n) {
  # The load data command does not permit use of a variable (that could be modified using concat() in a proc) as the infile
  # Therefore, modify the 'LNAppeals00.csv' portion of the following load data command and execute for each file to be imported
  # Be careful to observe the reported number of records written, skipped, and errors (use show warnings;)
  dbGetQuery(db, paste(
    "load data local infile '/home/tjb48/projects/lexnex/data-2019-12-03/LNAppeals", sprintf("%02d", n), ".csv'
     into table ImportRec fields terminated by ',' optionally enclosed by '\"' lines terminated by '\n' ignore 1 rows
     (@col1, @col2, @col3, @col4, @col5, @col6, @col7, @col8, @col9, @col10, @col11, @col12, @col13, @col14, @col15, @col16,
      @col17, @col18, @col19, @col20, @col21, @col22, @col23, @col24, @col25)
     set LNI=@col1, DecisionDate=trim(@col2), CourtShortName=@col3, CourtLongName=@col4, CaseTitleShort=@col6,
         CaseTitleLong=left(@col7, 2000), CaseTitleLexisNexis=@col5, CaseType=@col9, PubStatus=@col10,
         Panel=replace(@col11, 'urn:entity:', ''), PanelText=@col13, OpinionBy=@col14, OpinionByText=@col15,
         ConcurBy=@col16, ConcurByText=@col17, DissentBy=@col18, DissentByText=@col19,
         PerCuriam=@col20, Outcome=@col21, LegalTopics=replace(@col23, 'urn:topic:', ''), CoreTerms=@col24;", sep=""))
}

omitErroneousRecords <- function() {
  # Omit known erroneous records
  # Identify non-first appearance of case headers with duplicated LNI
  dbGetQuery(db, "drop table if exists ImportRec2;")
  dbGetQuery(db, "create table ImportRec2(LNI varchar(50), ID int, primary key(LNI, ID))")
  dbGetQuery(db, "insert into ImportRec2 select LNI, min(ID) from ImportRec group by LNI;")
  d <- dbGetQuery(db, "
    delete a
    from   ImportRec a join ImportRec2 b on a.LNI=b.LNI
    where  a.ID<>b.ID or a.LNI in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00');")
  dbGetQuery(db, "drop table if exists ImportRec2;")
  return(d)
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

createCaseExtRecords <- function() {
  dbGetQuery(db, "
    insert into CaseHeaderExt(LNI, PanelText, OpinionByText, ConcurByText, DissentByText, CoreTerms)
    select LNI,
           case when(character_length(PanelText)>0)then PanelText else null end,
           case when(character_length(OpinionByText)>0)then OpinionByText else null end,
           case when(character_length(ConcurByText)>0)then ConcurByText else null end,
           case when(character_length(DissentByText)>0)then DissentByText else null end,
           case when(character_length(CoreTerms)>0)then CoreTerms else null end
    from   ImportRec;")
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

createCaseTypeRecords <- function() {
  # Assigned case outcome type based on appearance of key words (reversed, vacated, affirmed, denied) in opinion
  dbGetQuery(db, "
    insert into CaseOutcomeType
    select LNI,
           case when(Outcome like '%reversed%')then 'Reversed'
                when(Outcome like '%vacated%')then 'Vacated'
                when(Outcome like '%affirmed%')then 'Affirmed'
                when(Outcome like '%denied%')then 'Denied'
                else 'Other'
           end
    from CaseHeader;")
}

createPanelRecords <- function() {
  dbGetQuery(db, "
    insert into Panel(LNI, JudgeID)
    select i.LNI, trim(substring_index(substring_index(i.Panel, '|', k.k), '|', -1))
    from   ImportRec i join k on k<=character_length(i.Panel)-character_length(replace(i.Panel, '|', ''))+1
    where  character_length(i.Panel)>0;")
}

createCaseLegalTopicsRecords <- function() {
  # Duplicates exist, so save uniques
  dbGetQuery(db, "
    insert into CaseLegalTopics(LNI, TopicID)
    select distinct i.LNI, trim(substring_index(substring_index(i.LegalTopics, '|', k.k), '|', -1))
    from   ImportRec i join k on k<=character_length(i.LegalTopics)-character_length(replace(i.LegalTopics, '|', ''))+1
    where  character_length(i.LegalTopics)>0;")
}

createShepardTreatmentRecords <- function() {

  # Create reference list of Shepard's Treatment codes (letters, categories, definitions)
  # Note the naming of input cols as @coln, with assignment to actual table cols using "set"

  dbGetQuery(db, "truncate table ShepardTreatment;")
  dbGetQuery(db, "
    load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/ShepardTreatment.csv'
    into table ShepardTreatment fields terminated by ',' optionally enclosed by '\"' lines terminated by '\\r\\n' ignore 1 rows
    (@col1, @col2, @col3, @col4, @col5, @col6, @col7, @col8) set Letter=trim(@col3), CurrentCategory=trim(@col4),
    SubCategory=trim(@col1), CaseStatute=trim(@col5), TreatmentPhrase=trim(@col2), Description=trim(@col8),
    UsageNotes=trim(@col6), ShepardSignal=trim(@col7);")
  
  # show warnings;
  # dbGetQuery(db, "select * from ShepardTreatment;")
  # dbGetQuery(db, "select CurrentCategory, count(1) as n from ShepardTreatment group by CurrentCategory;")
  # dbGetQuery(db, "select Letter, count(1) as n from ShepardTreatment group by Letter;")
  
  # Omit records labeled "obsolete"
  # Note that this omits letters referenced in Citation records
  dbGetQuery(db, "set sql_safe_updates=0;")
  dbGetQuery(db, "delete from ShepardTreatment where CurrentCategory='Obsolete';")
  
  # Remove extraneous spaces form letters
  dbGetQuery(db, "update ShepardTreatment set Letter=replace(Letter, ' ', '');")
  
  #dbGetQuery(db, "select Letter, count(1) as n from ShepardTreatment group by Letter having count(1)>1;")
  
  # Omit duplicate letter codes Acq and Noacq)
  # Note that several codes are duplicated by case, but with different definitions - assignment of
  # Treatment ID differentiates these
  dbGetQuery(db, "
    delete a
    from   ShepardTreatment a
           join (select   Letter, min(ID) as ID
                 from     ShepardTreatment
                 where    Letter in('Acq', 'Noacq')
                 group by Letter) b on a.Letter=b.Letter and a.ID<>b.ID;")
  
}

createCitationRecords <- function() {

  # Note that the 2019-03-13 version of source records have no column header records in row 1 and
  # virtual column three (the NormCite column) contains both commas and double quotes
  # A column header is assumed to be included, all double quotes removed from text, and virtual
  # column three surrounded by double quotes to preserve commas within the NormCite text

  #######################################################################################
  # From SQL DB creation script:
  #######################################################################################
  # Double quote third virtual column (NormCite)
  # Note that 2019-03-13 source IndexLNI file does not contain column headers
  # Some NormCite entries contain double quotes, so replace existing ones with !
  #######################################################################################

  # x <- scan(unz("LNItoLNItreatments.zip", "LNItoLNItreatments.csv"), what="character", sep="\n")
  # head(x)

  # f0 <- unz("LNItoLNItreatments.zip", "LNItoLNItreatments.csv")
  # f1 <- gzfile(f1<-"Citation.csv.gz")
  # x <- scan(f0, what="character", sep="\n", quote="", comment.char="", strip.white=T)
  # # Inspect row 1 (where column IDs would be expected)
  # x[1]
  # # Substitute double quotes
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
  #######################################################################################
  # End of citation SQL DB script excerpt
  #######################################################################################

  dbGetQuery(db, "drop table if exists ImportRec2;")
  dbGetQuery(db, "
    create table ImportRec2(
    ID int not null auto_increment primary key,
    LNI varchar(50) not null,
    LNICited varchar(50) not null,
    NormCitation varchar(100) not null,
    ShepardTreatment varchar(10) not null);")

  dbGetQuery(db, "load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/Citation.csv'
    into table ImportRec2 fields terminated by ',' optionally enclosed by '\"' lines terminated by '\\r\\n' ignore 1 rows
    (@col1, @col2, @col3, @col4) set LNI=@col1, LNICited=@col2, NormCitation=@col3, ShepardTreatment=@col4;")

  #dbGetQuery(db, "show warnings;")

  # Identify letters with no corresponding treatment record
  # Note that standard equality (=, in etc.) is case insensitive
  #dbGetQuery(db, "
  #  select   ShepardTreatment, count(1) as nrec
  #  from     ImportRec2
  #  where    ShepardTreatment not in(select Letter from ShepardTreatment)
  #  group by ShepardTreatment;")
  #dbGetQuery(db, "select * from ImportRec2 where ShepardTreatment not in(select Letter from ShepardTreatment) limit 100;")
  #dbGetQuery(db, "select * from ImportRec2 where ShepardTreatment like '#%' limit 100;")

  # Remove extraneous spaces form treatment letters
  dbGetQuery(db, "update ImportRec2 set ShepardTreatment=replace(ShepardTreatment, ' ', '');")

  # Identify duplicate import records
  #dbGetQuery(db, "
  #  select   LNI, LNICited, NormCitation, ShepardTreatment, count(1) as n
  #  from     ImportRec2
  #  group by LNI, LNICited, NormCitation, ShepardTreatment
  #  having   count(1)>1;")

  # Count unrecognized treatment letters
  # Case sensitive and identical length method
  #dbGetQuery(db, "
  #  select   i.ShepardTreatment, count(1) as nrec
  #  from     ImportRec2 i left join ShepardTreatment s on character_length(i.ShepardTreatment)=character_length(s.Letter)
  #                                                        and (i.ShepardTreatment regexp binary s.Letter)=1
  #  where    s.Letter is null
  #  group by ShepardTreatment;")

  # Create Citation records for all recognized Shepard's letters
  dbGetQuery(db, "truncate table Citation;")
  dbGetQuery(db, "
    insert into Citation(LNI, LNICited, NormCitation, ShepardTreatmentID)
    select i.LNI, i.LNICited, i.NormCitation, s.ID
    from   ImportRec2 i join ShepardTreatment s on character_length(i.ShepardTreatment)=character_length(s.Letter)
                                                   and (i.ShepardTreatment regexp binary s.Letter)=1;")

  # Report LNI-LNI relationships with difference in imported and Citation records
  #dbGetQuery(db, "
  #  select   a.LNI, a.LNICited, a.NormCitation, a.ShepardTreatment, b.n1, b.n2, b.n1-b.n2 as diff
  #  from     ImportRec2 a
  #           join (select a.LNI, a.LNICited, a.NormCitation, a.n as n1, b.n as n2
  #                 from (select   LNI, LNICited, NormCitation, count(1) as n
  #                       from     ImportRec2
  #                       where    ShepardTreatment in(select Letter from ShepardTreatment)
  #                       group by LNI, LNICited, NormCitation
  #                      ) a join (select   LNI, LNICited, NormCitation, count(1) as n
  #                                from     Citation
  #                                group by LNI, LNICited, NormCitation
  #                               ) b on a.LNI=b.LNI and a.LNICited=b.LNICited and a.NormCitation=b.NormCitation
  #                      where  a.n<>b.n
  #                 ) b on a.LNI=b.LNI and a.LNICited=b.LNICited and a.NormCitation=b.NormCitation
  #order by a.LNI, a.LNICited, a.NormCitation, a.ShepardTreatment;")

  # Identify duplicate Citation records (note that LNI-LNI relationships may have multiple treatment letters)
  #dbGetQuery(db, "
  #  select   LNI, LNICited, NormCitation, count(1) as n
  #  from     Citation
  #  group by LNI, LNICited, NormCitation
  #  having   count(1)>1;")

  #dbGetQuery(db, "select count(1) from ImportRec2;")
  #dbGetQuery(db, "select count(1) from Citation;")

  dbGetQuery(db, "drop table ImportRec2;")

}

createJudgeRecords <- function() {

  dbGetQuery(db, "drop table if exists ImportRec2;")
  dbGetQuery(db, "create table ImportRec2(
    ID int not null auto_increment primary key,
    x varchar(100) not null,
    JudgeID varchar(100) not null,
    Name varchar(200) not null);")

  dbGetQuery(db, "
    load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/Judges.csv'
    into table ImportRec2 fields terminated by ',' optionally enclosed by '\"' lines terminated by '\\r\\n' ignore 1 rows
    (@col1, @col2, @col3) set JudgeID=@col2, Name=@col3;")

  #dbGetQuery(db, "select * from ImportRec2 limit 25;")
  #dbGetQuery(db, "select * from ImportRec2 where character_length(replace(ImportRec.Name, ' ', ''))=0;")

  dbGetQuery(db, "
    insert into Judge(ID, Name)
    select trim(replace(JudgeID, 'urn:entity:', '')), left(trim(Name), 100)
    from   ImportRec2
    where  character_length(replace(Name, ' ', ''))>0;")

  dbGetQuery(db, "drop table ImportRec2;")

}

createLegalTopicsRecords <- function() {

  dbGetQuery(db, "drop table if exists ImportRec2;")
  dbGetQuery(db, "create table ImportRec2(
    ID int not null auto_increment primary key,
    guid varchar(50) not null,
    Hierarchy varchar(1000) not null,
    Note varchar(1000) not null,
    Status varchar(10) not null);")

  dbGetQuery(db, "
    load data local infile '/home/tjb48/projects/lexnex/data-2019-08-10/LegalTopics.csv'
    into table ImportRec2 fields terminated by ',' optionally enclosed by '\"' lines terminated by '\\r\\n' ignore 1 rows
    (@col1, @col2, @col3, @col4, @col5) set guid=@col5, Hierarchy=@col1, Note=@col2, Status=@col3;")

  #dbGetQuery(db, "select * from ImportRec2 limit 25;")

  # Duplicate IDs exist
  # Retain first entry for each ID
  dbGetQuery(db, "drop table if exists ImportRec3;")
  dbGetQuery(db, "create table ImportRec3(ID int primary key)")
  dbGetQuery(db, "insert into ImportRec3 select min(ID) from ImportRec2 group by guid;")
  
  dbGetQuery(db, "
    insert into LegalTopics(ID, Hierarchy, Note, Status)
    select a.guid, a.Hierarchy, a.Note, a.Status
    from   ImportRec2 a join ImportRec3 b on a.ID=b.ID")

  dbGetQuery(db, "drop table ImportRec2;")
  dbGetQuery(db, "drop table ImportRec3;")

}

createOpinionRecords <- function() {
  # Retrieve unique combinations of LNI and judge, since some case records are duplicated (3S4X-1450-003B-G54N-00000-00)
  dbGetQuery(db, "
    insert into Opinion(LNI, OpinionType, JudgeID)
    select distinct LNI, 'Opinion', OpinionBy
    from   ImportRec;")
  dbGetQuery(db, "
    insert into Opinion(LNI, OpinionType, JudgeID)
    select distinct LNI, 'Concur', ConcurBy
    from   ImportRec;")
  dbGetQuery(db, "
    insert into Opinion(LNI, OpinionType, JudgeID)
    select distinct LNI, 'Dissent', DissentBy
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
  db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals2",
                  # showPrompt() generates error on v 3.4.2
                  #user=rstudioapi::showPrompt("Duke Law Appeals", "User ID:  ", Sys.info()["user"]),
                  user="tjb48",
                  password=rstudioapi::askForPassword("Password:  "))
} else {
  # Local connection
  db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals2", user="tjb48", password="")
}

# sink(file="LNImport-2020-01-11.log", type="output")
# sink(file=file("LNImport-2020-01-11.log", open="wa"), type="message")

createTables()
createkIndices()

i <- 0
t0 <- proc.time()[3]
while(i<12) {

  d <- createImportRec()
  if(nrow(d)!=0)
    stop(paste("Error in createImportRec:\n", d[1,1], sep=""))
  cat(i, "createImportRec\n")

  d <- loadData(i)
  if(nrow(d)!=0) {
    stop(paste("Error in loadData:\n", d[1,1], sep=""))
  } else {
    # Suppress warning of "unsigned integer imported as numeric" message issued by dbGetQuery()
    # This is related to the result set returned by dbGetQuery() and not with the MySQL load data statement
    d <- suppressWarnings(dbGetQuery(db, "show warnings"))
    if(nrow(d)!=0)
      print(d)
  }
  cat(i, "loadData\n")

  d <- omitErroneousRecords()

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

  d <- createCaseExtRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create case extension recs:\n", d[1,1], sep=""))
  cat(i, "create case extension recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createCaseTypeRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create case type recs:\n", d[1,1], sep=""))
  cat(i, "create case type recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createPanelRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create panel recs:\n", d[1,1], sep=""))
  cat(i, "create panel recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  d <- createCaseLegalTopicsRecords()
  if(nrow(d)!=0)
    stop(paste("Error in create case legal topics recs:\n", d[1,1], sep=""))
  cat(i, "create case legal topics recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  #d <- createLNTopicsRecords()
  #if(nrow(d)!=0)
  #  stop(paste("Error in create LN topics recs:\n", d[1,1], sep=""))
  #cat(i, "create LN topics recs  ", format(Sys.time(), "%X"), "  ", proc.time()[3]-t0, "\n")

  i <- i+1

}

createJudgeRecords()
createShepardTreatmentRecords()
createCitationRecords()
createLegalTopicsRecords()

# Independent load of opinions
# Note that some source records reference opinion, concurring, and dissenting judges by
# judge ID, other records list textual judge names
i <- 0
t0 <- proc.time()[3]
while(i<12) {

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
# dbGetQuery(db, "create unique index CaseHeaderLNI on CaseHeader(LNI);")
dbGetQuery(db, "create index CaseHeaderDecisionDate on CaseHeader(DecisionDate);")
dbGetQuery(db, "create index CaseHeaderCourtID on CaseHeader(CourtID);")
dbGetQuery(db, "create index CaseHeaderCaseTitleShort on CaseHeader(CaseTitleShort);")
dbGetQuery(db, "create index CaseHeaderPubStatus on CaseHeader(PubStatus);")
dbGetQuery(db, "create index CaseHeaderPerCuriam on CaseHeader(PerCuriam);")
dbGetQuery(db, "create unique index CourtShortName on Court(ShortName);")
dbGetQuery(db, "create unique index CourtLongName on Court(LongName);")
dbGetQuery(db, "create unique index CaseTypeLNICaseType on CaseType(LNI, CaseType);")
dbGetQuery(db, "create index CaseTypeCaseType on CaseType(CaseType);")
dbGetQuery(db, "create index CaseOutcomeTypeOutcomeType on CaseOutcomeType(OutcomeType);")
dbGetQuery(db, "create index PanelJudgeID on Panel(JudgeID);")
dbGetQuery(db, "create index INOpinionType on Opinion(OpinionType);")
dbGetQuery(db, "create index INOpinionJudgeID on Opinion(JudgeID);")
dbGetQuery(db, "create index INCaseLegalTopicsID on CaseLegalTopics(TopicID);")
dbGetQuery(db, "create index INLegalTopicsStatus on LegalTopics(Status);")
dbGetQuery(db, "create index INShepardTreatmentLetter on ShepardTreatment(Letter);")
dbGetQuery(db, "create index INShepardTreatmentCurrentCategory on ShepardTreatment(CurrentCategory);")
dbGetQuery(db, "create index INShepardTreatmentSubCategory on ShepardTreatment(SubCategory);")
dbGetQuery(db, "create index INShepardTreatmentCaseStatute on ShepardTreatment(CaseStatute);")
dbGetQuery(db, "create index INShepardTreatmentTreatmentPhrase on ShepardTreatment(TreatmentPhrase);")
dbGetQuery(db, "create index INShepardTreatmentShepardSignal on ShepardTreatment(ShepardSignal);")
dbGetQuery(db, "create unique index INShepardTreatmentUnique on ShepardTreatment(Letter, CurrentCategory, SubCategory, CaseStatute)")
dbGetQuery(db, "create index INCitationLNI on Citation(LNI);")
dbGetQuery(db, "create index INCitationLNICited on Citation(LNICited);")
dbGetQuery(db, "create index INCitationNormCitation on Citation(NormCitation);")
dbGetQuery(db, "create index INCitationShepardTreatmentID on Citation(ShepardTreatmentID);")
dbGetQuery(db, "create unique index INCitationUnique on Citation(LNI, LNICited, NormCitation, ShepardTreatmentID);")
# The following index cannot be created due to multiple non-primary reporters
# MySQL does not support rollbacks within triggers, so enforcement of uniqe(LNI, PrimaryReporter) does not appear possible
# create unique index INCitationUniqueLNIPrimaryReporter on Citation(LNI, LNICited, PrimaryReporter);
dbGetQuery(db, "create index INCitationPrimaryReporter on Citation(PrimaryReporter);")

dbGetQuery(db, "alter table CaseHeader add constraint FKCaseHeaderCourt foreign key (CourtID) references Court(ID);")
dbGetQuery(db, "alter table CaseHeaderExt add constraint FKCaseHeaderExtLNI foreign key (LNI) references CaseHeader(LNI);")
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
dbGetQuery(db, "alter table Citation add constraint FKCitationShepardTreatmentID foreign key (ShepardTreatmentID) references ShepardTreatment(ID);")


####################################################################################################
#### Disconnect
####################################################################################################

dbDisconnect(db)
