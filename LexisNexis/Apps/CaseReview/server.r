#####################################################################################################
# Duke University Law School
# LexisNexis Data Review App, June 2020
# Shiny server script
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetwork/visnetwork.pdf

#####################################################################################################
# Data source:  UCD database developed by University of Nebraska and Duke University
#####################################################################################################

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(DBI)
library(RMySQL)
library(DT)

##########################################################################################################
# Shiny server function
##########################################################################################################

shinyServer(
  function(input, output, session) {

    db <<- ""

    ##########################################################################################################
    # Database connection
    ##########################################################################################################
    
    observeEvent(input$dbConnect,{

      # Connect to DB
      #db <<- try(dbConnect(MySQL(), host="127.0.0.1", dbname="Appeals2", user="lexnexReader", password=input$dbPW))
      db <<- try(dbConnect(MySQL(), host="127.0.0.1", port=3306, dbname="Appeals2", user="lexnexReader", password=input$dbPW))
      if(class(db)=="MySQLConnection") {
        output$dbMsg <- renderText(NULL)
        # Retrieve court identifiers
        courtList <- dbGetQuery(db, "select id, ShortName as court from Court")
        courtList[,"court"] <- sub("Circuit Court of Appeals", "Circ", courtList[,"court"])
        courtList[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", courtList[,"court"])
        courtList[,"court"] <- sub("Court of Federal Claims", "Fed Claims", courtList[,"court"])
        courtList[,"court"] <- sub("Circuit Bankruptcy Appellate Panel", "Circ Bkruptcy", courtList[,"court"])
        courtList[,"court"] <- sub("Judicial Conference, Committee on Judicial Conduct", "Jud Conduct", courtList[,"court"])
        courtList[,"court"] <- sub("Temporary Emergency Court of Appeals", "Temp Emergency", courtList[,"court"])
        courtList[,"court"] <- sub("Tennessee Eastern District Court", "Tenn E Dist", courtList[,"court"])
        courtList[,"court"] <- sub("Texas Southern District Court", "Tex S Dist", courtList[,"court"])
        # Assign order of court labels
        k <- as.integer(gsub("[a-zA-Z]", "", courtList[,"court"]))
        kna <- which(is.na(k))
        k <- c(order(k)[1:(length(k)-length(kna))], kna[order(courtList[kna,"court"])])
        # Replace empty text and append court ID
        courtList[which(nchar(courtList[,"court"])==0),"court"] <- "empty"
        courtList[,"court"] <- paste(courtList[,"court"], " (", courtList[,"id"], ")", sep="")
        # Assign court values to selection list
        # Prepend "All" to indicate all courts
        updateSelectInput(session, "queryCourt", choices=setNames(c("All", courtList[k,"id"]), c("All", courtList[k,"court"])))
      } else {
        db <<- ""
        output$dbMsg <- renderText(HTML("<font color=red>Cannot connect to DB</font>"))
      }

    }, ignoreInit=T)

    ##########################################################################################################
    # Query
    ##########################################################################################################
    
    observeEvent(input$query,{

      if(class(db)=="MySQLConnection") {

        output$queryMsg <- renderText(NULL)
        output$tableCaseHeader <- renderTable(NULL)
        output$extData <- renderText(NULL)
        output$tableLegalTopics <- renderTable(NULL)
        output$tableCitations <- renderTable(NULL)

        # Query observations
        sql <- paste(" select a.LNI, c.ShortName as Court, a.DecisionDate, a.CaseTitleShort,",
                     "        case ct.cvcr when(1)then 'civil' when(2)then 'criminal' when(3)then 'civ-crim' else '' end as CaseType,",
                     "        a.PubStatus, case a.PerCuriam when(1)then 'yes' when(0)then 'no' end as PerCuriam, a.Outcome",
                     " from   CaseHeader a join Court c on a.CourtID=c.ID",
                     "        left join (select   LNI,",
                     "                            sum(case CaseType when('civil')then 1 when('criminal')then 2 else 0 end) as cvcr",
                     "                   from     CaseType",
                     "                   group by LNI",
                     "                  ) ct on a.LNI=ct.LNI",
                     " where  1=1",
                     ifelse(input$queryLNI!="", paste(" and a.LNI='", input$queryLNI, "'", sep=""), ""),
                     ifelse(input$queryCourt!="All", paste(" and a.CourtID=", input$queryCourt, sep=""), ""),
                     ifelse(length(input$queryDate0)>0, paste(" and a.DecisionDate>='", input$queryDate0[1], "'", sep=""), ""),
                     ifelse(length(input$queryDate1)>0, paste(" and a.DecisionDate<='", input$queryDate1[1], "'", sep=""), ""),
                     ifelse(input$queryTitleText!="", paste(" and a.CaseTitleShort like '%", input$queryTitleText, "%'", sep=""), ""),
                     ifelse(input$queryOutcomeText!="", paste(" and a.Outcome like '%", input$queryOutcomeText, "%'", sep=""), ""),
                     ifelse(input$queryPubStatus!="all", paste(" and a.PubStatus='", input$queryPubStatus, "'", sep=""), ""),
                     ifelse(input$queryPerCuriam!="all", paste(" and a.PerCuriam=", ifelse(input$queryPerCuriam=="yes", 1, 0), sep=""), ""),
                     sep="")
        #print(sql)
        tableCaseHeader <<- try(dbGetQuery(db, sql))
        if(class(tableCaseHeader)=="data.frame") {
          # Render table
          output$tableCaseHeader <-
            DT::renderDataTable(
              datatable(tableCaseHeader,
                        # Suppress row names
                        # Automatically hide nav buttons when rows less than pages per row
                        rownames=F, autoHideNavigation=T,
                        # Use row selection
                        selection=list(mode="single", target="row"),
                        # Include column filters ("bottom" will display them below table)
                        filter="top",
                        # Configure other table options
                        # Information on data tables options available at https://rstudio.github.io/DT/options.html
                        options=list( # Set rows per page
                                      pageLength=5, lengthMenu=c(1, 5, 10, 25, 50, 100),
                                      # Allow regex style searches
                                      search=list(regex=T, caseInsensitive=T),
                                      # Let DT set col widths (basically to width of max entry)
                                      autoWidth=T )

                       ))
        } else {
          output$queryMsg <- renderText(HTML(paste("<font color=red>", tableCaseHeader[1], "</font>", sep="")))
        }

      } else {

        output$queryMsg <- renderText(HTML("<font color=red>No DB connection</font>"))

      }

    }, ignoreInit=T)

    #######################################################################################################
    # Case header table row selection
    #######################################################################################################

    observeEvent(input$tableCaseHeader_rows_selected, {

      if(length(input$tableCaseHeader_rows_selected)>0) {

        # Retrieve and display extended data
        # Note that one extension record exists for each LNI in case header records
        ext <- dbGetQuery(db, paste("select * from CaseHeaderExt where lni='",
                                    tableCaseHeader[input$tableCaseHeader_rows_selected,], "'", sep=""))
        output$extData <- renderText(HTML(paste("<u>panel</u><br> ", ext[,"PanelText"], "<br><br>",
                                                "<u>opinion by:</u><br> ", ext[,"OpinionByText"], "<br><br>",
                                                "<u>concur by:</u><br> ", ext[,"ConcurByText"], "<br><br>",
                                                "<u>dissent by:</u><br> ", ext[,"DissentByText"], sep="")))

        # Retrieve and display legal topics
        topics <- dbGetQuery(db, paste("select b.Note as legal_topic from Appeals.CaseLegalTopics a join LegalTopics b on a.TopicID=b.ID where a.lni='",
                                       tableCaseHeader[input$tableCaseHeader_rows_selected,], "' order by Note", sep=""))
        if(nrow(topics)>0) {
          output$tableLegalTopics <- renderTable(topics)
        } else {
          output$tableLegalTopics <- renderTable(NULL)
        }

        # Retrieve and display citations
        citations <- dbGetQuery(db, paste(" select   a.LNICited, NormCitation, b.Letter, b.CurrentCategory, b.SubCategory,",
                                          "          b.CaseStatute, b.TreatmentPhrase",
                                                     #b.Description, b.UsageNotes, b.ShepardSignal",
                                          " from     Citation a join ShepardTreatment b on a.ShepardTreatmentID=b.ID",
                                          " where    a.lni='", tableCaseHeader[input$tableCaseHeader_rows_selected,], "'",
                                          " order by b.Letter", sep=""))
        if(nrow(citations)>0) {
          output$tableCitations <- renderTable(citations)
        } else {
          output$tableCitations <- renderTable(NULL)
        }
      }

    })

  }
)