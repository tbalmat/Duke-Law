# Duke University Law Appeals Analysis
# Review of 2019-03-13 LexisNexi Data

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(DBI)
library(RMySQL)

setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis")

##################################################
# Connect to Appeals database
##################################################

if(substring(R.Version()[["os"]], 1, 5)=="mingw") {
  # Use ssh port 3306 forwarding from clients
  # Mac:  ssh -L 3306:127.0.0.1:3306 user@lexnex-smben-01.oit.duke.edu
  # Windows:  Install PuTTY and configure a connection for Host Name lexnex-smben-01.oit.duke.edu
  #           Under ssh, Source port=3306, Destination=127.0.0.1:3306
  db <- dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals",
                  # showPrompt() generates error on v 3.4.2
                  #user=rstudioapi::showPrompt("Duke Law Appeals", "User ID:  ", Sys.info()["user"]),
                  user="tjb48",
                  password=rstudioapi::askForPassword("Password:  "))
} else {
  # Local connection
  db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals", user="tjb48", password="")
}

##################################################
# Query
##################################################

# Byung-koo's example
# Notet the use of CaseHeader.ID and Citation.ID in joining cases and citations
# These IDs have no relatoinship and should not be used in joins
x <- dbGetQuery(db,
     "select CaseHeader.ID, year(DecisionDate) as year, CaseHeader.LNI, CaseHeader.CaseTitleShort, Citation.LNICited,
             ShepardTreatment.TreatmentPhrase, ShepardTreatment.ShepardSignal
      from   CaseHeader, Citation, ShepardTreatment
      where  CaseHeader.ID = Citation.ID
             and year(CaseHeader.DecisionDate) = 2000
             and Citation.ShepardTreatmentID = ShepardTreatment.ID")

# Count rows returned and cases in 2000
nrow(x)
dbGetQuery(db, "select count(1) from CaseHeader where year(DecisionDate)=2000")

# Here is what, it is expected that BK intended (case joined to citations using LNI)
y <- dbGetQuery(db,
     "select ch.ID, year(ch.DecisionDate) as year, ch.LNI, ch.CaseTitleShort, ci.LNICited,
             st.TreatmentPhrase, st.ShepardSignal
      from   CaseHeader ch join Citation ci on ch.LNI=ci.LNI
             join ShepardTreatment st on ci.ShepardTreatmentID=st.ID
      where  year(ch.DecisionDate)=2000")

nrow(y)
