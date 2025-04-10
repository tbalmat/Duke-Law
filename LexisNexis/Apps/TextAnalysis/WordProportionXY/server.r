# Duke University Law Appeal Text Analysis

# Shiny service module

options(stringsAsFactors=F)
library(shiny)
library(DBI)
library(RMySQL)
library(XML)
library(ggplot2)
library(igraph)
library(ggraph)
library(grid)
library(ggrepel)

shinyServer(

  function(input, output, session) {

    # Function:  execute query
    execQuery <- function(sql) {
      # Use ssh port 3306 forwarding
      # Mac:  ssh -L 3306:127.0.0.1:3306 user@lexnex-smben-01.oit.duke.edu
      # Windows:  Install PuTTY and configure a connection for Host Name lexnex-smben-01.oit.duke.edu
      #           Under ssh, Source port=3306, Destination=127.0.0.1:3306
      #db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals", user="lexnexReader", password="lnread")
      d <- sqlQuery(db, sql)
      #dbDisconnect(db)
      return(d)
    }

    # Function:  retrieve outcome text for specified case type and outcome type
    outcomeWords <- function(caseType, outcomeType, omitWords) {
      sql <- paste(" select ch.Outcome",
                   " from   CaseHeader ch join CaseTypeComposite cty on ch.LNI=cty.LNI",
                   "        join CaseOutcomeType oty on ch.LNI=oty.LNI",
                   " where  1=1",
                   ifelse(caseType!="", paste(" and cty.CaseType='", caseType, "'", sep=""), ""),
                   ifelse(outcomeType!="", paste(" and oty.OutcomeType='", outcomeType, "'", sep=""), ""), sep="")
      outcome <- dbGetQuery(db, sql)[,1]
      # Omit punctuation, special symbols, numerals, pronouns, and words of length < three
      w <- unlist(lapply(unlist(strsplit(tolower(outcome), " ")), function(a)  gsub("[^a-z]", "", a)))
      k <- which(nchar(w)>2 & !w %in% omitWords)
      return(w[k])
    }

    # Function:  plot word proportions of one case type against another 
    plotcaseType <- function(savePng=F) {

      output$msg <- renderText("")

      qtype <- c(input$caseType1, input$caseType2)

      # Aggregate proportions for both classes
      w <- outcomeWords(caseType=qtype[1], outcomeType="", omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
      w <- outcomeWords(caseType=qtype[2], outcomeType="", omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

      print(paste("min-max p(type 1, type 2): ", min(w1[,"p"]), " ", max(w1[,"p"]), " ", min(w1[,"p"]), " ", max(w2[,"p"]), sep=""))

      # Join by word
      w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
      colnames(w) <- c("term", "p1", "p2")

      # Convert NA to 0
      w[which(is.na(w[,"p1"])),"p1"] <- 0
      w[which(is.na(w[,"p2"])),"p2"] <- 0

      # Filter by p
      prange <- input$pRange
      k <- which(w[,"p1"]>=prange[1] & w[,"p1"]<=prange[2] | w[,"p2"]>=prange[1] & w[,"p2"]<=prange[2])

      print(paste("n-points: ", length(k), sep=""))

      g <- ggplot() +
        geom_abline(intercept=min(min(w[k,"p1"], w[k,"p2"])), slope=1, color="gray75") +
        geom_point(data=w[k,], aes(x=p1, y=p2), color="blue3", alpha=0.35) +
        geom_text_repel(data=w[k,], aes(label=term, x=p1, y=p2), size=3, segment.size=0.25, segment.color="gray50") +
        theme(plot.title=element_text(size=12, hjust=0.5),
              plot.subtitle=element_text(size=10, hjust=0.5),
              plot.caption=element_text(size=12, hjust=0.5),
              panel.background=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_rect(fill=NA, color="gray75"),
              panel.spacing.x=unit(0, "lines"),
              axis.title.x=element_text(size=10),
              axis.title.y=element_text(size=10),
              axis.text.x=element_text(size=8),
              axis.text.y=element_text(size=8),
              strip.text=element_text(size=8),
              strip.background=element_blank(),
              legend.position="bottom",
              legend.background=element_rect(color="gray"),
              legend.key=element_rect(fill="white"),
              legend.box="horizontal",
              legend.text=element_text(size=8),
              legend.title=element_text(size=8)) +
        labs(title="Lexis Nexis Appeals Outcome Text",
             subtitle=paste("Pairwise Word Proportions for Case Classes ", names(caseType)[which(caseType==qtype[2])],
                            " and ", names(caseType)[which(caseType==qtype[1])], "\n", sep=""),
             x=paste("\n", names(caseType)[which(caseType==qtype[1])], sep=""),
             y=paste(names(caseType)[which(caseType==qtype[2])], "\n", sep=""))

        # Render
        if(!savePng) {
          output$wordProportionXY <- renderPlot(g)

        } else {
          png(paste(input$graphDir, "\\OutcomeWordDistribution-", qtype[2], "-by-", qtype[1], "-p-", prange[1], "-", prange[2],
                    ".png", sep=""), res=300, height=2400, width=2400)
          print(g)
          dev.off()
        }

    }

    # Function:  plot word proportions of one class against another 
    plotoutcomeType <- function(savePng=F) {

      output$msg <- renderText("")

      qtype <- c(input$outcomeType1, input$outcomeType2)

      # Aggregate proportions for both classes
      w <- outcomeWords(caseType="", outcomeType=qtype[1], omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
      w <- outcomeWords(caseType="", outcomeType=qtype[2], omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

      print(paste(" min-max p(type 1, type 2): ", min(w1[,"p"]), " ", max(w1[,"p"]), " ", min(w1[,"p"]), " ", max(w2[,"p"]), sep=""))

      # Join by word
      w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
      colnames(w) <- c("term", "p1", "p2")

      # Convert NA to 0
      w[which(is.na(w[,"p1"])),"p1"] <- 0
      w[which(is.na(w[,"p2"])),"p2"] <- 0

      # Filter by p
      prange <- input$pRange
      k <- which(w[,"p1"]>=prange[1] & w[,"p1"]<=prange[2] & w[,"p2"]>=prange[1] & w[,"p2"]<=prange[2])

      print(paste("n-points: ", length(k), sep=""))

      g <- ggplot() +
        geom_abline(intercept=min(min(w[k,"p1"], w[k,"p2"])), slope=1, color="gray75") +
        geom_point(data=w[k,], aes(x=p1, y=p2), color="blue3", alpha=0.35) +
        geom_text_repel(data=w[k,], aes(label=term, x=p1, y=p2), size=3, segment.size=0.25, segment.color="gray50") +
        theme(plot.title=element_text(size=12, hjust=0.5),
              plot.subtitle=element_text(size=10, hjust=0.5),
              plot.caption=element_text(size=12, hjust=0.5),
              panel.background=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_rect(fill=NA, color="gray75"),
              panel.spacing.x=unit(0, "lines"),
              axis.title.x=element_text(size=10),
              axis.title.y=element_text(size=10),
              axis.text.x=element_text(size=8),
              axis.text.y=element_text(size=8),
              strip.text=element_text(size=8),
              strip.background=element_blank(),
              legend.position="bottom",
              legend.background=element_rect(color="gray"),
              legend.key=element_rect(fill="white"),
              legend.box="horizontal",
              legend.text=element_text(size=8),
              legend.title=element_text(size=8)) +
        labs(title="Lexis Nexis Appeal Outcome Text",
             subtitle=paste("Pairwise Word Proportions for Opinion Types ", names(outcomeType)[which(outcomeType==qtype[2])],
                            " and ", names(outcomeType)[which(outcomeType==qtype[1])], "\n", sep=""),
             x=paste("\n", names(outcomeType)[which(outcomeType==qtype[1])], sep=""),
             y=paste(names(outcomeType)[which(outcomeType==qtype[2])], "\n", sep=""))

        # Render
        if(!savePng) {
          output$wordProportionXY <- renderPlot(g)
        } else {
          png(paste(input$graphDir, "\\OutcomeWordDistribution-", qtype[2], "-by-", qtype[1], "-p-", prange[1], "-", prange[2],
                    ".png", sep=""), res=300, height=2400, width=2400)
          print(g)
          dev.off()
        }

    }

    # Reactive environment events
    observeEvent(
      input$actionViewPlot1, {
        plotcaseType(savePng=F)
      }
    )
    observeEvent(
      input$actionSavePlot1, {
        plotcaseType(savePng=T)
      }
    )
    observeEvent(
      input$actionViewPlot2, {
        plotoutcomeType(savePng=F)
      }
    )
    observeEvent(
      input$actionSavePlot2, {
        plotoutcomeType(savePng=T)
      }
    )

    # Execution begins here

    # Construct case class and opinion type options
    #q <- dbGetQuery("select distinct CasetType, CaseType as Description from CaseType order by CaseType")
    #caseType <- setNames(q[,1], q[,2])
    caseType <- c("Criminal"="Criminal", "Civil"="Civil", "CriminalCivil"="CriminalCivil", "None"="None")
    #q <- execQuery("select Type, Description from OpinionType order by Type")
    outcomeType <- c("Affirmed"="Affirmed", "Reversed"="Reversed", "Denied"="Denied", "Vacated"="Vacated", "Other"="Other")
    updateSelectInput(session, "caseType1", choices=caseType, selected=caseType[1])
    updateSelectInput(session, "caseType2", choices=caseType, selected=caseType[2])
    updateSelectInput(session, "outcomeType1", choices=outcomeType, selected=outcomeType[1])
    updateSelectInput(session, "outcomeType2", choices=outcomeType, selected=outcomeType[2])

    # Construct list of key words, either to be omitted from or identified in text
    pronouns <- tolower(scan("..\\..\\Pronouns.csv", what="character"))
    omitWords <- tolower(scan("..\\..\\OmitWords.csv", what="character"))
    latinLegal <- tolower(xmlToDataFrame("..\\..\\LatinLegalTerms.xml")[,1])

  }
)
