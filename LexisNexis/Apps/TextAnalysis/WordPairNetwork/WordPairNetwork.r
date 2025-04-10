library(shiny)
library(DBI)
library(RMySQL)

db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals", user="lexnexReader", password="lnread")

runApp(appDir="C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\ModelsAnalysis\\OutcomeTextAnalysis\\OutcomeTextAnalysisApp\\WordPairNetwork",
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       #port=4291,
       display.mode = c("auto", "normal", "showcase"))

dbDisconnect(db)