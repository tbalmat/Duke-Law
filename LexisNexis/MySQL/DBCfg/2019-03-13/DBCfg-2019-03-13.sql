-- ----------------------------------------------------------------------------------------------------------------
-- Duke University Law Appeals Analysis Database Configuration
-- Source Data Provided by LexisNexis March 13, 2019
-- Script Author:  Tom Balmat, Research Computing, Duke University OIT, thomas.balmat@duke.edu
-- ----------------------------------------------------------------------------------------------------------------

-- Enable load data statement

-- Windows:
-- Report current secure-file-priv value
-- select @@GLOBAL.secure_file_priv;
-- Include the following statement in c:\ProgramData\MySQL\MySQL Server 8.0\my.ini
-- Restart server after mod
-- secure-file-priv="C:\Projects\Duke\Law\LexisNexisCaseAnalysis\LexisNexisData-2019-03-13"
-- Permit import from any directory
-- secure-file-priv=""

-- Red Hat (MariaDB):
-- select @@GLOBAL.local_infile;
-- set global local_infile=1;

-- ----------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------
-- Create database and tables
-- ----------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------

-- drop database Appeals
create database Appeals;

use Appeals;

drop table if exists CaseHeader;
create table CaseHeader(
ID int not null auto_increment primary key,
LNI varchar(50) not null,
DecisionDate date null,
CourtID smallint not null,
CaseTitleShort varchar(500) not null,
CaseTitleLong varchar(2000) not null,
CaseTitleLexisNexis varchar(500) not null,
PubStatus varchar(25) not null,
PerCuriam bit not null default 0,
Outcome varchar(2000) null);

drop table if exists Court;
create table Court(
ID smallint not null auto_increment primary key,
ShortName varchar(50) not null,
LongName varchar(100) not null);

drop table if exists CaseType;
create table CaseType(
LNI varchar(50) not null,
CaseType varchar(25) not null,
primary key(LNI, CaseType));

drop table if exists Judge;
create table Judge(
ID varchar(25) not null primary key,
Name varchar(100) null);

drop table if exists Panel;
create table Panel(
LNI varchar(50) not null,
JudgeID varchar(25) not null,
primary key(LNI, JudgeID));

drop table if exists Opinion;
create table Opinion(
LNI varchar(50) not null,
OpinionType varchar(25) not null,
JudgeID varchar(25) not null,
primary key(LNI, OpinionType, JudgeID));

drop table if exists CaseLegalTopics;
create table CaseLegalTopics(
LNI varchar(50) not null,
TopicID varchar(50) not null,
primary key(LNI, TopicID));

drop table if exists CaseLNTopics;
create table CaseLNTopics(
LNI varchar(50) not null,
TopicID varchar(25) not null,
primary key(LNI, TopicID));

drop table if exists LegalTopics;
create table LegalTopics(
TopicID varchar(50) not null primary key,
Description varchar(1000));

drop table if exists LNTopics;
create table LNTopics(
TopicID varchar(25) not null primary key,
Description varchar(1000));

drop table if exists ShepardTreatment;
create table ShepardTreatment(
ID smallint not null auto_increment primary key,
Letter varchar(10) not null,
CurrentCategory varchar(50) not null,
SubCategory varchar(50) not null,
CaseStatute varchar(25) not null,
TreatmentPhrase varchar(100) not null,
Description varchar(1000) not null,
UsageNotes varchar(1000) not null,
ShepardSignal varchar(100) not null);

drop table if exists Citation;
create table Citation(
ID int not null auto_increment primary key,
LNI varchar(50) not null,
LNICited varchar(50) not null,
NormCitation varchar(100) not null,
PrimaryReporter bit not null default 0,
ShepardTreatmentID varchar(10) not null);


-- ----------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------
-- Import source data
-- ---------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------

-- --------------------
-- Shepard's Treatment (letters, categories, definitions)
-- Note the naming of input cols as @coln, with assignment to actual table cols using "set"
-- The "local" modifier is required on Ubuntu (to force local file sys search), but generates an error under Windows
-- --------------------

truncate table ShepardTreatment;
-- load data infile 'C:/Projects/Duke/Law/LexisNexisCaseAnalysis/LexisNexisData-2019-03-13/ShepardTreatment.csv'
load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/ShepardTreatment.csv'
into table ShepardTreatment fields terminated by ',' optionally enclosed by '"' lines terminated by '\r\n' ignore 1 rows
(@col1, @col2, @col3, @col4, @col5, @col6, @col7, @col8) set Letter=trim(@col3), CurrentCategory=trim(@col4),
SubCategory=trim(@col1), CaseStatute=trim(@col5), TreatmentPhrase=trim(@col2), Description=trim(@col8),
UsageNotes=trim(@col6), ShepardSignal=trim(@col7);

-- show warnings;
-- select * from ShepardTreatment;
-- select CurrentCategory, count(1) as n from ShepardTreatment group by CurrentCategory;
-- select Letter, count(1) as n from ShepardTreatment group by Letter;

-- Omit records labeled "obsolete"
-- Note that this omits letters referenced in Citation records
delete ShepardTreatment where CurrentCategory='Obsolete';

-- Remove extraneous spaces form letters
update ShepardTreatment set Letter=replace(Letter, ' ', '');

-- select Letter, count(1) as n from ShepardTreatment group by Letter having count(1)>1;

-- Omit duplicate letter codes Acq and Noacq)
-- Note that several codes are duplicated by case, but with different definitions - assignment of
-- Treatment ID differentiates these)
delete a
from   ShepardTreatment a
       join (select   Letter, min(ID) as ID
             from     ShepardTreatment
             where    Letter in('Acq', 'Noacq')
             group by Letter) b on a.Letter=b.Letter and a.ID<>b.ID;

create index INShepardTreatmentLetter on ShepardTreatment(Letter);
create index INShepardTreatmentCurrentCategory on ShepardTreatment(CurrentCategory);
create index INShepardTreatmentSubCategory on ShepardTreatment(SubCategory);
create index INShepardTreatmentCaseStatute on ShepardTreatment(CaseStatute);
create index INShepardTreatmentTreatmentPhrase on ShepardTreatment(TreatmentPhrase);
create index INShepardTreatmentShepardSignal on ShepardTreatment(ShepardSignal);
create unique index INShepardTreatmentUnique on ShepardTreatment(Letter, CurrentCategory, SubCategory, CaseStatute) 

-- --------------------
-- Citations
-- --------------------

-- Note that the 2019-03-13 version of source records have no column header records in row 1 and
-- virtual column three (the NormCite column) contains both commas and double quotes
-- A column header is assumed to be included, all double quotes removed from text, and virtual
-- column three surrounded by double quotes to preserve commas within the NormCite text

drop table if exists ImportRec;
create table ImportRec(
ID int not null auto_increment primary key,
LNI varchar(50) not null,
LNICited varchar(50) not null,
NormCitation varchar(100) not null,
ShepardTreatment varchar(10) not null);

load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/Citation.csv'
into table ImportRec fields terminated by ',' optionally enclosed by '"' lines terminated by '\r\n' ignore 1 rows
(@col1, @col2, @col3, @col4) set LNI=@col1, LNICited=@col2, NormCitation=@col3, ShepardTreatment=@col4;

-- show warnings;

-- Identify letters with no corresponding treatment record
-- Note that standard equality (=, in etc.) is case insensitive
-- select   ShepardTreatment, count(1) as nrec
-- from     ImportRec
-- where    ShepardTreatment not in(select Letter from ShepardTreatment)
-- group by ShepardTreatment;
-- select * from ImportRec where ShepardTreatment not in(select Letter from ShepardTreatment) limit 100;
-- select * from ImportRec where ShepardTreatment like '#%' limit 100;

-- Remove extraneous spaces form treatment letters
update ImportRec set ShepardTreatment=replace(ShepardTreatment, ' ', '');

-- Identify duplicate import records
select   LNI, LNICited, NormCitation, ShepardTreatment, count(1) as n
from     ImportRec
group by LNI, LNICited, NormCitation, ShepardTreatment
having   count(1)>1;

-- Count unrecognized treatment letters
-- Case sensitive and identical length method
select   i.ShepardTreatment, count(1) as nrec
from     ImportRec i left join ShepardTreatment s on char_length(i.ShepardTreatment)=char_length(s.Letter)
                                                     and (i.ShepardTreatment regexp binary s.Letter)=1
where    s.Letter is null
group by ShepardTreatment;

-- Create Citation records for all recognized Shepard's letters
truncate table Citation;
insert into Citation(LNI, LNICited, NormCitation, ShepardTreatmentID)
select i.LNI, i.LNICited, i.NormCitation, s.ID
from   ImportRec i join ShepardTreatment s on char_length(i.ShepardTreatment)=char_length(s.Letter)
                                              and (i.ShepardTreatment regexp binary s.Letter)=1;

-- Report LNI-LNI relationships with difference in imported and Citation records
select   a.LNI, a.LNICited, a.NormCitation, a.ShepardTreatment, b.n1, b.n2, b.n1-b.n2 as diff
from     ImportRec a
         join (select a.LNI, a.LNICited, a.NormCitation, a.n as n1, b.n as n2
               from (select   LNI, LNICited, NormCitation, count(1) as n
                     from     ImportRec
                     where    ShepardTreatment in(select Letter from ShepardTreatment)
                     group by LNI, LNICited, NormCitation
                    ) a join (select   LNI, LNICited, NormCitation, count(1) as n
                              from     Citation
                              group by LNI, LNICited, NormCitation
                             ) b on a.LNI=b.LNI and a.LNICited=b.LNICited and a.NormCitation=b.NormCitation
                    where  a.n<>b.n
               ) b on a.LNI=b.LNI and a.LNICited=b.LNICited and a.NormCitation=b.NormCitation
order by a.LNI, a.LNICited, a.NormCitation, a.ShepardTreatment;

-- Identify duplicate Citation records (note that LNI-LNI relationships may have multiple treatment letters)
select   LNI, LNICited, NormCitation, count(1) as n
from     Citation
group by LNI, LNICited, NormCitation
having   count(1)>1;

-- select count(1) from ImportRec;
-- select count(1) from Citation;

create index INCitationLNI on Citation(LNI);
create index INCitationLNICited on Citation(LNICited);
create index INCitationNormCitation on Citation(NormCitation);
create index INCitationShepardTreatmentID on Citation(ShepardTreatmentID);
create unique index INCitationUnique on Citation(LNI, LNICited, NormCitation, ShepardTreatmentID);
-- The following index cannot be created due to multiple non-primary reporters
-- MySQL does not support rollbacks within triggers, so enforcement of uniqe(LNI, PrimaryReporter) does not appear possible
-- create unique index INCitationUniqueLNIPrimaryReporter on Citation(LNI, LNICited, PrimaryReporter);
create index INCitationPrimaryReporter on Citation(PrimaryReporter);
alter table Citation add constraint FKCitationShepardTreatmentID foreign key (ShepardTreatmentID) references ShepardTreatment(ID);

drop table ImportRec;


-- --------------------
-- Judges
-- --------------------

create table ImportRec(
ID int not null auto_increment primary key,
x varchar(100) not null,
JudgeID varchar(100) not null,
Name varchar(200) not null);

load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/Judges.csv'
into table ImportRec fields terminated by ',' optionally enclosed by '"' lines terminated by '\r\n' ignore 1 rows
(@col1, @col2, @col3) set JudgeID=@col2, Name=@col3;

-- select * from ImportRec limit 25;
-- select * from ImportRec where character_length(replace(ImportRec.Name, ' ', ''))=0;

insert into Judge(ID, Name)
select trim(replace(JudgeID, 'urn:entity:', '')), left(trim(Name), 100)
from   ImportRec
where  character_length(replace(ImportRec.Name, ' ', ''))>0;

drop table ImportRec;


-- ---------------------------------------------------------------------------------------------------------------------
-- Case records:  header, court, type, panel, and topics
-- Although all source records could be imported followed by court, type, panel, and topics parsing, the total number
-- of records causes the DB engine too slow significantly
-- A more efficient alternative is to drop ImportRec, load data, and parse separately for each infile
-- To do this, modify nn in the "load data infile ... LNAppealsnn.csv" instruction then execute from
-- dropping and creating table ImportRec to parsing and saving LNTopics 
-- ---------------------------------------------------------------------------------------------------------------------

-- Generate rows of consecutive integers from 1 to 65,536
-- These are used in delimited text parsing operations
drop table if exists k;
create table k(k int auto_increment primary key, x tinyint);
insert into k(x) select a.k
from   (select 1 as k union all select 1 union all select 1 union all select 1) a
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) b
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) c
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) d
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) e
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) f
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) g
       cross join (select 1 as k union all select 1 union all select 1 union all select 1) h;
--select count(1) from k;

drop table if exists ImportRec;
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
LNTopics varchar(10000) null);

-- The load data command does not permit use of a variable (that could be modified using concat() in a proc) as the infile
-- Therefore, modify the 'LNAppeals00.csv' portion of the following load data command and execute for each file to be imported
-- Be careful to observe the reported number of records written, skipped, and errors (use show warnings;)
load data local infile '/home/tjb48/projects/lexnex/data-2019-03-13/LNAppeals00.csv'
into table ImportRec fields terminated by ',' optionally enclosed by '"' lines terminated by '\n' ignore 1 rows
(@col1, @col2, @col3, @col4, @col5, @col6, @col7, @col8, @col9, @col10, @col11, @col12, @col13, @col14, @col15, @col16, @col17, @col18, @col19, @col20)
set LNI=@col1, DecisionDate=trim(@col2), CourtShortName=@col3, CourtLongName=@col4, CaseTitleShort=@col6,
    CaseTitleLong=left(@col7, 2000), CaseTitleLexisNexis=@col5, CaseType=@col9, PubStatus=@col10, Panel=@col11,
    OpinionBy=@col13, ConcurBy=@col14, DissentBy=@col15, PerCuriam=@col16, Outcome=@col17, LegalTopics=@col18, LNTopics=@col20;

-- show warnings;

-- Omit known erroneous records
-- 3T3Y-TS60-0038-X1MJ-00000-00:  rec 31213 of Part01, opinion text appears in OpinionBy
-- 3T8C-CSW0-0038-X3XN-00000-00:  rec 19963 of Part08, opinion text appears in OpinionBy
delete from ImportRec where LNI in('3T3Y-TS60-0038-X1MJ-00000-00', '3T8C-CSW0-0038-X3XN-00000-00');

-- Validate some columns
-- select distinct year(DecisionDate) from ImportRec order by 1;
-- select LNI, DecisionDate, CaseTitleShort from ImportRec where year(DecisionDate)=0;
-- select distinct CaseType from ImportRec;
-- select truncate(character_length(LegalTopics)/1000, 0), count(1) as n from ImportRec group by truncate(character_length(LegalTopics)/1000, 0);

-- --------------------
-- Court records
-- --------------------

insert into Court(ShortName, LongName)
select distinct trim(CourtShortName), trim(CourtLongName)
from   ImportRec i left join Court c on i.CourtShortName=c.ShortName and i.CourtLongName=c.LongName
where  c.ID is null;

-- Validate dates (note that source format of valid dates is YYYY-MM-DD)
-- select DecisionDate from ImportRec where character_length(DecisionDate)<>10;
-- select DecisionDate from ImportRec where character_length(DecisionDate)-character_length(replace(DecisionDate, '-', ''))<>2;
-- select DecisionDate from ImportRec where substring(DecisionDate, 5, 1)<>'-' or substring(DecisionDate, 8, 1)<>'-';
-- select DecisionDate from ImportRec where convert(left(DecisionDate, 4), unsigned) not between 1974 and 2018;
-- select DecisionDate from ImportRec where convert(substring(DecisionDate, 6, 2), unsigned) not between 1 and 12;
-- select DecisionDate
-- from   (select DecisionDate,
               -- convert(left(DecisionDate, 4), unsigned) as yr,
               -- convert(substring(DecisionDate, 6, 2), unsigned) as mo,
               -- convert(substring(DecisionDate, 9, 2), unsigned) as dy
        -- from   ImportRec) a
-- where   mo not between 1 and 12
        -- or mo in(1, 3, 5, 7, 8, 10, 12) and dy not between 1 and 31
        -- or mo in(4, 6, 9, 11) and dy not between 1 and 30
        -- or mo=2 and dy not between 1 and 29
        -- or mo=2 and dy=29 and yr not in(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016);

-- --------------------
-- Case header records
-- --------------------

insert into CaseHeader(LNI, DecisionDate, CourtID, CaseTitleShort, CaseTitleLong, CaseTitleLexisNexis,
                       PubStatus, PerCuriam, Outcome)
select i.LNI, case when(character_length(replace(i.DecisionDate, ' ', ''))>0)then i.DecisionDate else NULL end,
       c.ID, left(trim(i.CaseTitleShort), 500), left(trim(i.CaseTitleLong), 2000),
       left(trim(i.CaseTitleLexisNexis), 500), trim(i.PubStatus),
       case when(character_length(replace(i.PerCuriam, ' ', ''))>0)then 1 else 0 end,
       case when(character_length(trim(i.Outcome))>0)then left(trim(i.Outcome), 2000) else null end
from   ImportRec i join Court c on trim(i.CourtShortName)=c.ShortName and trim(i.CourtLongName)=c.LongName;

-- Distribution of dates by year
-- select year(DecisionDate), count(1) as n from CaseHeader group by year(DecisionDate);

--  select LNI from CaseHeader where CourtID=15;


-- --------------------
-- Case type
-- --------------------

-- Parse case type and save individual values in records with reference to case ID
-- Given n case types, generate n copies by joinining to integer (k) records
-- Use substring_index to extract text preceding kth delimiter (inner call using k) then
-- use substring_index again to extract text trailing last delimiter in inner text (outer call with parameter -1)
-- Note that the index parameter of substring instructs to return all text prior to the kth delimiter (from the left)
-- when k is positive and all text following the kth delimiter (from the right) when k is negative
-- Note, also, the use of length(text with delimiters)-length(text without delimiters) to compute number
-- of delimiters (which is one less than the required number of text record copies) 
insert into CaseType(LNI, CaseType)
select i.LNI, trim(substring_index(substring_index(i.CaseType, '|', k.k), '|', -1))
from   ImportRec i join k on k<=character_length(i.CaseType)-character_length(replace(i.CaseType, '|', ''))+1
where  character_length(i.CaseType)>0;

-- --------------------
-- Panels
-- --------------------

-- Parse and save judges in panel records
insert into Panel(LNI, JudgeID)
select i.LNI, trim(substring_index(substring_index(i.Panel, '|', k.k), '|', -1))
from   ImportRec i join k on k<=character_length(i.Panel)-character_length(replace(i.Panel, '|', ''))+1
where  character_length(i.Panel)>0;

-- --------------------
-- Legal topics
-- --------------------

-- Parse and save case legal topics
-- Duplicates exist, so save uniques
insert into CaseLegalTopics(LNI, TopicID)
select distinct i.LNI, trim(substring_index(substring_index(i.LegalTopics, '|', k.k), '|', -1))
from   ImportRec i join k on k<=character_length(i.LegalTopics)-character_length(replace(i.LegalTopics, '|', ''))+1
where  character_length(i.LegalTopics)>0;

-- --------------------
-- LN Topics
-- --------------------

-- Parse and save case LexisNexis topics
insert into CaseLNTopics(LNI, TopicID)
select distinct i.LNI, trim(substring_index(substring_index(i.LNTopics, '|', k.k), '|', -1))
from   ImportRec i join k on k<=character_length(i.LNTopics)-character_length(replace(i.LNTopics, '|', ''))+1
where  character_length(i.LNTopics)>0;

drop table k;
drop table ImportRec;

create unique index CaseHeaderLNI on CaseHeader(LNI);
create index CaseHeaderDecisionDate on CaseHeader(Class);
create index CaseHeaderCourtID on CaseHeader(CourtID);
create index CaseHeaderCaseTitleShort on CaseHeader(CaseTitleShort);
create index CaseHeaderPubStatus on CaseHeader(PubStatus);
create index CaseHeaderPerCuriam on CaseHeader(PerCuriam);
create unique index CourtShortName on Court(ShortName);
create unique index CourtLongName on Court(LongName);
create unique index CaseTypeLNICaseType on CaseType(LNI, CaseType);
create index CaseTypeCaseType on CaseType(CaseType);
create index PanelJudgeID on Panel(JudgeID);
create index INOpinionType on Opinion(OpinionType);
create index INOpinionJudgeID on Opinion(JudgeID);
create index INCaseLegalTopicsID on CaseLegalTopics(TopicID);
create index INLegalTopicsDescription on LegalTopics(Description);
create index INCaseLNTopicsID on CaseLNTopics(TopicID);
create index INLNTopicsDescription on LNTopics(Description);

alter table CaseHeader add constraint FKCaseHeaderCourt foreign key (CourtID) references Court(ID);
alter table CaseType add constraint FKCaseTypeLNI foreign key (LNI) references CaseHeader(LNI);
alter table Panel add constraint FKPanelLNI foreign key (LNI) references CaseHeader(LNI);
alter table Panel add constraint FKPanelJudgeID foreign key (JudgeID) references Judge(ID);
alter table Opinion add constraint FKOpinionLNI foreign key (LNI) references CaseHeader(LNI);
alter table Opinion add constraint FKOpinionJudgeID foreign key (JudgeID) references Judge(ID);
alter table Opinion add constraint CKOpinionType check(OpinionType in('Opinion', 'Concur', 'Dissent'));
alter table CaseLegalTopics add constraint FKCaseLegalTopicsLNI foreign key (LNI) references CaseHeader(LNI);
alter table CaseLegalTopics add constraint FKCaseLegalTopicsTopicID foreign key (TopicID) references LegalTopics(ID);
alter table CaseLNTopics add constraint FKCaseLNTopicsLNI foreign key (LNI) references CaseHeader(LNI);
alter table CaseLNTopics add constraint FKCaseLNTopicsTopicID foreign key (TopicID) references LNTopics(ID);
alter table Citation add constraint FKCitationLNI foreign key (LNI) references CaseHeader(LNI);
alter table Citation add constraint FKCitationLNICited foreign key (LNICited) references CaseHeader(LNI);

-- List foreign keys
-- select * from information_schema.key_column_usage;

-- ----------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------
-- Create a few useful views
-- These are for example only
-- Since modifications to this database are expected to be infrequent, construction of static tables with content
-- as appears in views is a preferred method (to avoid repeated queries of virtually static records)
-- ----------------------------------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------------------

-- -----------------------------------
-- Case composite type view
-- -----------------------------------

-- create view CaseTypeCount as-
--   -- Accumulate case types by case
--   select    LNI, count(1),
--             sum(case when(CaseType='criminal')then 1 else 0 end) as nCriminal,
--             sum(case when(CaseType='civil')then 1 else 0 end) as nCivil
--   from      CaseType
--   group  by LNI;
--                      
-- create view CaseTypeComposite as
--   select ch.LNI,
--          case when(nty.LNI is not null)then
--                 concat(case when(nty.nCriminal>0)then 'Criminal' else '' end,
--                        case when(nty.nCivil>0)then 'Civil' else '' end)
--               else 'None'
--           end as CaseType
--   from   CaseHeader ch left join CaseTypeCount nty on ch.LNI=nty.LNI;


-- -----------------------------------
-- Case composite type table
-- -----------------------------------

create table CaseTypeComposite(LNI varchar(50) not null, CaseType varchar(25) not null, primary key(LNI, CaseType));
insert into CaseTypeComposite
select ch.LNI,
       case when(nty.LNI is not null)then
              concat(case when(nty.nCriminal>0)then 'Criminal' else '' end,
                     case when(nty.nCivil>0)then 'Civil' else '' end)
            else 'None'
       end as CaseType
from   CaseHeader ch
       left join ( -- Accumulate case types by case
                   select    LNI,
                             sum(case when(CaseType='criminal')then 1 else 0 end) as nCriminal,
                             sum(case when(CaseType='civil')then 1 else 0 end) as nCivil
                   from      CaseType
                   group  by LNI
                 ) nty on ch.LNI=nty.LNI;

-- select LNI from CaseHeader where LNI not in(select LNI from CaseTypeComposite);

-- -----------------------------------
-- Outcome type table
-- -----------------------------------

create table CaseOutcomeType(LNI varchar(50) not null, OutcomeType varchar(25) not null, primary key(LNI, OutcomeType));
insert into CaseOutcomeType
select LNI,
       case when(Outcome like '%reversed%')then 'Reversed'
            when(Outcome like '%vacated%')then 'Vacated'
            when(Outcome like '%affirmed%')then 'Affirmed'
            when(Outcome like '%denied%')then 'Denied'
            else 'Other'
       end
from CaseHeader;

-- select LNI from CaseHeader where LNI not in(select LNI from OutcomeType);

-- -----------------------------------------------
-- Assign primary case reporter to each citation
-- -----------------------------------------------

 -- Information on official reporter designations is available at https://nyulaw.libguides.com/c.php?g=773843&p=5551618

 -- From this site:

 -- Federal case law will be divided between three branches: U.S. Supreme Court, U.S. Courts of Appeal (Circuit Courts),
 -- and the U.S. District Courts. 

 -- Case law is published in official and unofficial case law reporters, in chronological order. Supreme Court opinions
 -- are the only federal opinions published in official case reporters.  West is the major publisher of unofficial case
 -- reporters.

 -- Case citations are the easiest way to retrieve a case (it is possible to find cases by party name, but may not be
 -- as exact).  Case citations are structured by volume number, reporter abbreviation, and first page of the case. 

 -- U.S Supreme Court Cases: Supreme Court opinions are published in 3 different case law reporters:
 -- United States Reports (the official reporter) - "U.S.", Supreme Court Reporter (West) - "S. Ct.",
 -- and Lawyers' Edition (Lexis) - "L. Ed.".  Sample citation for Brown v. Board of Ed. of Topeka:
 -- 347 U.S. 483, 74 S. Ct. 686, 98 L. Ed. 873.

 -- Federal Circuit Courts of Appeal: Circuit Court opinions are published in unofficial reporters (West):
 -- Federal Reporter - "F.", Federal Reporter Second Series - "F.2d" and Federal Reporter Third Series - "F.3d".

 -- Federal District (Trial) Courts:  The District Court opinions are published in unofficial case reporters (West):
 -- Federal Supplement - "F. Supp." and Federal Supplement Second Series - "F. Supp.2d"

 -- Earlier Federal Cases (dating before the Federal Reporter series) are available in Federal Case "F. Cas." bound
 -- volumes

-- use Appeals;

-- Evaluate Supreme Court reporter(s)
-- Note that MySQL repeats [0-9] search parameters (as opposed to requiring standard regex [0-9]{n} for n repetitions, or digits here)
-- However, [0-9] does not match on 12a, 1a2, etc., so that alpha patterns are not included
select distinct NormCitation from Citation where NormCitation regexp binary '[0-9] S. Ct. [0-9]' = 1 order by 1;
select distinct NormCitation from Citation where NormCitation regexp '[^0-9] S. Ct. [^0-9]' = 1 order by 1;

-- Verify that all S. Ct. reporters are of form "n1 S. Ct. n2" where n1 is a 1 to 3 position integer and n2 is a 1 to 4 position integer
-- This should be equivalent to where NormCitation regexp '[^0-9] S. Ct. [^0-9]' = 1
select distinct NormCitation
from   Citation
where  NormCitation regexp ' S. Ct. ' = 1
       and replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(NormCitation, '0', ''),
                   '1', ''), '2', ''), '3', ''), '4', ''), '5', ''), '6', ''), '7', ''), '8', ''), '9', '') <> ' S. Ct. '
limit  100;       

-- Evaluate Federal District Court reporters (F.nd)
select distinct NormCitation from Citation where NormCitation regexp binary '[1-9] F.[1-9]d [0-9]'= 1 order by 1;
select distinct NormCitation from Citation where NormCitation regexp binary '[^1-9] F.[1-9]d [^0-9]'= 1 order by 1;

-- Evaluate U.S. reporters
select distinct NormCitation from Citation where NormCitation regexp binary '[0-9] U.S. [0-9]'= 1 order by 1;
select distinct NormCitation from Citation where NormCitation regexp binary '[^1-9] U.S. [^0-9]'= 1 order by 1;

SET SQL_SAFE_UPDATES=0;

-- Clear all primary reporters
update Citation set PrimaryReporter=0;

-- Mark Supreme Court primary reporters
update Citation set PrimaryReporter=1 where NormCitation regexp binary '[0-9] S. Ct. [0-9]' = 1;

-- Mark Federal Court primary reporters
update Citation set PrimaryReporter=1 where NormCitation regexp binary '[0-9] F.[1-9]d [0-9]' = 1;

-- Mark U.S. primary reporters
update Citation set PrimaryReporter=1 where NormCitation regexp binary '[0-9] U.S. [0-9]' = 1;

-- Test for duplicate primary reporters
-- Note that valid duplicates may occur due to multiple Shepard's treatments
-- select LNI, LNICited from Citation where PrimaryReporter=1 group by LNI, LNICited having count(1)>1 limit 100;

-- Test for duplicate primary reporter types by case
select   distinct LNI, LNICited
from     Citation
where    PrimaryReporter=1 and LNI in(select LNI from CaseHeader)
group by LNI, LNICited
having   sum(NormCitation regexp binary '[0-9] S. Ct. [0-9]')>0
         and sum(NormCitation regexp binary '[0-9] F.[1-9]d [0-9]')>0
         and sum(NormCitation regexp binary '[0-9] U.S. [0-9]')>0
limit    200;

-- Enumerate cases with at least one primary reporter
select count(distinct LNI) from CaseHeader where LNI in(select LNI from Citation where PrimaryReporter=1);

-- Enumerate cases that cite
select count(distinct LNI) from CaseHeader where LNI in(select LNI from Citation);

-- Tabulate cited cases by number of primary reporters
select   nSC+nFed+nUS+nOther, count(1)
from     ( select   LNICited,
                    case when(sum(SC )>0)then 1 else 0 end as nSC,
                    case when(sum(Fed)>0)then 1 else 0 end as nFed,
                    case when(sum(US)>0)then 1 else 0 end as nUS,
                    case when(sum(US)>0)then 1 else 0 end as nOther
           from     ( select LNI, LNICited, SC, Fed, US, case when(SC=0 and Fed=0 and US=0)then 1 else 0 end as Other
                      from   ( -- Retrieve unique citing and cited LNI pairs so that each is counted once only
                               -- Recall that multinple Shepard's tratments exist for pairs
                               select distinct LNI, LNICited,
                                      NormCitation regexp binary '[0-9] S. Ct. [0-9]' as SC,
                                      NormCitation regexp binary '[0-9] F.[1-9]d [0-9]' as Fed,
                                      NormCitation regexp binary '[0-9] U.S. [0-9]' as US
                               from   Citation
                               where  LNI in(select LNI from CaseHeader) and PrimaryReporter=1
                             ) a
                    ) a
           group by LNICited
         ) a
group by nSC+nFed+nUS+nOther;

-- Enumerate cited cases that do not exist in case header records
select count(distinct LNICited) from Citation where LNICited not in(select LNI from CaseHeader) and PrimaryReporter=1;
select count(distinct LNICited) from Citation;

-- Enumerate citations for case header cases, but with no primary reporter
select count(distinct LNI) from Citation where LNI in(select LNI from CaseHeader) and LNI not in(select LNI from Citation where PrimaryReporter=1);
select count(distinct LNI) from Citation;
select count(distinct LNI) from Citation where LNI not in(select LNI from CaseHeader);

-- Enumerate Case Headers with no primary reporter
select count(1) from CaseHeader where LNI not in(select LNI from Citation where PrimaryReporter=1);
select count(1) from CaseHeader where LNI not in(select LNI from Citation);

-- List normalized citations for cases with no primary reporter
select NormCitation from Citation where LNI not in(select LNI from Citation where PrimaryReporter=1) limit 100;

-- Tabulate normalized citations for cases with no primary reporter (collapse numbers to a single 9)
select   Reporter, count(1) as n
from     ( select replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(
                  NormCitation, '0', '9'), '1', '9'), '2', '9'), '3', '9'), '4', '9'), '5', '9'), '6', '9'), '7', '9'), '8', '9'),
                  '99999', '9'), '9999', '9'), '999', '9'), '99', '9') as Reporter
           from   Citation
           where  LNI in(select LNI from CaseHeader) and LNI not in(select LNI from Citation where PrimaryReporter=1)
         ) a
group by Reporter
order by 1;

-- Review Shepard's treatment codes for primary reporter citations
select * from ShepardTreatment where ID in(select distinct ShepardTreatmentID from Citation where PrimaryReporter=1);
