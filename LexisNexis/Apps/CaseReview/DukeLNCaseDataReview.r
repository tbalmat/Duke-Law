# Duke University Law School
# LexisNexis Data Review App, June 2020

# Launch app defined in ui.r and server.r in specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")
rm(list=ls())
gc()

# Specify directory containing ui.r and server.r
ad <- "C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\CaseDataReviewApp"

# Execute 
runApp(appDir=ad,
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4301,
       display.mode = c("auto", "normal", "showcase"))

