# Duke University Law Appeal Opinion Text Analysis

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

shinyServer(

  function(input, output, session) {

    # Function:  execute query
    execQuery <- function(sql) {
      # Use ssh port 3306 forwarding
      # Mac:  ssh -L 3306:127.0.0.1:3306 user@lexnex-smben-01.oit.duke.edu
      # Windows:  Install PuTTY and configure a connection for Host Name lexnex-smben-01.oit.duke.edu
      #           Under ssh, Source port=3306, Destination=127.0.0.1:3306
      #db <- dbConnect(MySQL(), host="localhost", port=3306, dbname="Appeals", user="lexnexReader", password="lnread")
      d <- dbGetQuery(db, sql)
      #dbDisconnect(db)
      return(d)
    }

    # Function:  retrieve opinion text for specified case class and opinion type
    # opWords <- function(caseClass="", opType="", omitWords) {

      # opText <- execQuery(paste(" select od.opinionText",
                                # " from   CaseHeader ch join OpinionHeader oh on ch.ID=oh.CaseID",
                                # "        join OpinionDetail od on oh.CaseID=od.CaseID",
                                # "        and oh.OpinionID=od.OpinionID",
                                # " where  1=1",
                                # ifelse(caseClass!="", paste(" and ch.Class='", class, "'", sep=""), ""),
                                # ifelse(opType!="", paste(" and oh.Type='", opType, "'", sep=""), ""), sep=""))[,1]
        #-- Omit punctuation, special symbols, numerals, pronouns, and words of length < three
        # w <- unlist(lapply(unlist(strsplit(tolower(opText), " ")), function(a)  gsub("[^a-z]", "", a)))
        # k <- which(nchar(w)>2 & !w %in% omitWords)
        # return(w[k])

    # }

    # Function:  retrieve outcome text for specified case type and outcome type
    outcomeWords <- function(caseType="", outcomeType="", omitWords) {

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

    # Function:  plot network of leading and trailing word pairs
    # Query opinion text and split into vector of words
    # Omit punctuation, numerals, pronouns, and words of length less than three
    # Convert to lower case
    # Return vector of words (default) or data.frame of three vectors:  words,
    # opinion record ID (concatenated case ID, opinion ID, opinion sub ID), and word
    # index within opinion record
    # Construct network graph using word pair counts and correlation of pairs

    generatePlots <- function(savePng=F) {

      output$msg <- renderText("")

      # Select case class, opinion type, and minimum edge frequency for inclusion 
      ktCase <- which(input$caseType==caseType)
      ktOutcome <- which(input$outcomeType==outcomeType)
      nedgethresh <- input$nedgethresh

      # Retrieve opinion paragraphs
      oText <- execQuery(paste(" select ch.Outcome",
                               " from   CaseHeader ch join CaseTypeComposite cty on ch.LNI=cty.LNI",
                               "        join CaseOutcomeType oty on ch.LNI=oty.LNI",
                               " where  1=1",
                                ifelse(length(ktCase)>0, paste(" and cty.CaseType='", input$caseType, "'", sep=""), ""),
                                ifelse(length(ktOutcome)>0, paste(" and oty.OutcomeType='", input$outcomeType, "'", sep=""), ""), sep=""))[,1]

      cat("A")

      if(length(oText)>0) {

        output$msg <- renderText(paste("Outcome records:  ", format(length(oText), big.mark=","), sep=""))

        # Split each paragraph into words
        # Omit punctuation, special symbols, numerals, pronouns, and words of length < three
        # Compose pairs of consecutive words 
        w <- do.call(rbind,
                     sapply(oText,
                       function(a) {
                         # Split words and omit punctuation, special characters, and unwanted words
                         w <- gsub("[^a-z]", "", unlist(strsplit(tolower(a), " ")))
                         w <- w[which(nchar(w)>2 & !w %in% omitWords)]
                         if(length(w)>1) {
                           return(t(sapply(1:(length(w)-1), function(i) w[i:(i+1)])))
                         } else if(length(w)==1) {
                           return(t(c(w[1], NA)))
                         } else {
                           return(t(c(NA, NA)))
                         }}))

        # Aggregate word pairs
        wpair <- aggregate(rep(1, nrow(w)), by=list(w[,1], w[,2]), sum)
        colnames(wpair) <- c("w1", "w2", "n12")

        cat("B")

        # Join to aggregated counts of leading and trailing word appearance
        wpair <- merge(wpair, setNames(aggregate(rep(1, nrow(w)), by=list(w[,1]), sum), c("w1", "n1")), by.x="w1", by.y="w1")
        wpair <- merge(wpair, setNames(aggregate(rep(1, nrow(w)), by=list(w[,2]), sum), c("w2", "n2")), by.x="w2", by.y="w2")[c("w1", "w2", "n1", "n2", "n12")]

        # Compose vector of word pairs with frequency (per 1,000 words) > threshold
        kg <- which(wpair[,"n12"]/sum(wpair[,"n12"])*1000>nedgethresh)

        if(length(kg)>0) {

          # Place word pairs in successive elements of wv
          n <- length(kg)
          wv <- rep("", 2*n)
          wv[seq(1, 2*n-1, 2)] <- wpair[kg,"w1"]
          wv[seq(2, 2*n, 2)] <- wpair[kg,"w2"]

          # Construct network from wv element pairs (successive elements)
          # Note the igraph:: qualifier for when igraph package not loaded
          x <- igraph::make_graph(wv)

          # Construct node circle radii for beginning and ending edge node elements
          # Use ratio of word count to max word count within beginning and ending groups
          kr <- match(vertex_attr(x, "name"), wpair[kg,"w1"])
          r1 <- wpair[kr,"n1"]/max(wpair[kr,"n1"], na.rm=T)
          kr <- match(vertex_attr(x, "name"), wpair[kg,"w2"])
          r2 <- wpair[kr,"n2"]/max(wpair[kr,"n2"], na.rm=T)

          # Aggregate frequencies by word (individual directions and sum of both directions)
          nfreq <- rep(0, length(vertex_attr(x, "name")))
          names(nfreq) <- vertex_attr(x, "name")
          for(i in kg) {
            #nfreq[wpair[i,"w1"]] <- nfreq[wpair[i,"w1"]]+wpair[i,"n1"]
            #nfreq[wpair[i,"w2"]] <- nfreq[wpair[i,"w2"]]+wpair[i,"n2"]
            nfreq[wpair[i,"w1"]] <- nfreq[wpair[i,"w1"]]+wpair[i,"n12"]
            nfreq[wpair[i,"w2"]] <- nfreq[wpair[i,"w2"]]+wpair[i,"n12"]
          }

          # Compute correlation of words in each pair
          n11 <- wpair[kg,"n12"]
          n00 <- sum(wpair[,"n12"])-wpair[kg,"n1"]-wpair[kg,"n2"]+wpair[kg,"n12"]
          n10 <- wpair[kg,"n1"]-wpair[kg,"n12"]
          n01 <- wpair[kg,"n2"]-wpair[kg,"n12"]
          n1. <- wpair[kg,"n1"]
          n0. <- sum(wpair[,"n12"])-wpair[kg,"n1"]
          n.0 <- sum(wpair[,"n12"])-wpair[kg,"n2"]
          n.1 <- wpair[kg,"n2"]
          ecorr <- (n11*n00 - n10*n01) / sqrt(n1.*n0.*n.0*n.1) 

          # Compose matrix of beginning and ending nodes, one row per edge, beg in col 1, end in col 2
          enode <- do.call(rbind, sapply(as.matrix(attr(E(x), "vnames")), function(e) strsplit(e, "\\|")))

          # Use unique names for each graph, otherwise the final graph generated appears in all frames, even
          # if a common graph is removed between generation

          # Plot network of word pairs (colors indicate total in-out node frequency)
          g1 <- ggraph(x, layout="fr") +
            # One plot element per edge
            geom_edge_link(color="gray60",
                           start_cap=circle(r=2.5, "mm"),
                           end_cap=circle(r=2.5, "mm"),
                           arrow=arrow(type="closed", length=unit(1.5, "mm"))) +
            # One plot element per node
            # Note that size parameters are strictly in units if mm
            geom_node_point(aes(color=nfreq/sum(nfreq)*1000), size=5, alpha=1) +
            scale_color_gradient(name="n / 1000 words", low="#2222FF", high="#FF4444") +
            geom_node_text(aes(label=name), repel=T, point.padding=unit(1, "mm")) +
            theme_void() +
            theme(plot.margin=unit(c(0.5, 0.25, 0.5, 0.25), "in"), legend.box.spacing=unit(0.35, "in"))

          # Render
          if(!savePng) {
            output$wordPairNetFreq <- renderPlot(g1)
          } else {
            png(paste(input$graphDir, "\\OutcomeWordPairNetwork-Frequency-CaseType-",
                      ifelse(length(ktCase)>0, input$caseType, "All"), ,
                      "-OutcomeType-", ifelse(length(ktOutcome)>0, input$outcomeType, "All"),
                      "-Min-Edge-Freq-", nedgethresh, ".png", sep=""),
                res=300, height=3000, width=3000)
            print(g1)
            dev.off()
          }

          # Plot word pair correlation (edge color indicates correlation)

          g2 <- ggraph(x, layout="fr") +
            # One plot element per edge
            geom_edge_link(aes(color=ecorr,
                               start_cap=circle(r=2+2*(nfreq[enode[,1]]-min(nfreq[enode[,1]]))/(max(nfreq[enode[,1]])-min(nfreq[enode[,1]])), "mm"),
                               end_cap=circle(r=2+2*(nfreq[enode[,2]]-min(nfreq[enode[,2]]))/(max(nfreq[enode[,2]])-min(nfreq[enode[,2]])), "mm")),
                           width=1, arrow=grid::arrow(type="closed", length=unit(1.5, "mm"))) +
            scale_edge_color_gradient(name="edge-r", low="#2222FF", high="#FF4444") +
            # One plot element per node
            geom_node_point(aes(size=nfreq/sum(nfreq)*1000), shape=1) +
            # Size parameters are in strict mm units
            scale_size_continuous(name="in-out node freq (per 1,000)", range = c(1, 7)) +
            geom_node_text(aes(label=name), repel=T, point.padding=unit(1, "mm")) +
            theme_void() +
            theme(plot.margin=unit(c(0.5, 0.25, 0.5, 0.25), "in"), legend.box.spacing=unit(0.35, "in"))

          # Render
          if(!savePng) {
            output$wordPairNetCorr <- renderPlot(g2)
          } else {
            png(paste(input$graphDir, "\\WordPairNetwork-Correlation-CaseType-",
                      ifelse(length(ktCase)>0, input$caseType, "All"), ,
                      "-OutcomeType-", ifelse(length(ktOutcome)>0, input$outcomeType, "All"),
                      "-Min-Edge-Freq-", nedgethresh, ".png", sep=""),
                      res=300, height=3000, width=3000)
            print(g2)
            dev.off()
          }

        } else {

          output$wordPairNetFreq <- renderPlot(NULL)
          output$wordPairNetCorr <- renderPlot(NULL)
          output$msg <- renderText("No word pairs retrieved with frequency above requested threshold")

        }

      } else {

        output$wordPairNetFreq <- renderPlot(NULL)
        output$wordPairNetCorr <- renderPlot(NULL)
        output$msg <- renderText("No case records retrieved for requested case class and opinion type combination")

      }

    }

    # Reactive environment events
    observeEvent(
      input$actionViewPlots, {
        generatePlots(savePng=F)
      }
    )

    observeEvent(
      input$actionSavePlots, {
        generatePlots(savePng=T)
      }
    )

    # Execution begins here

    # Construct case class and opinion type options
    #q <- execQuery("select Class, Description from CaseClass order by Class")
    #caseClass <- setNames(q[,1], q[,2])
    #q <- execQuery("select Type, Description from OpinionType order by Type")
    #opType <- setNames(q[,1], q[,2])
    caseType <- c("Criminal"="Criminal", "Civil"="Civil", "CriminalCivil"="CriminalCivil", "None"="None")
    outcomeType <- c("Affirmed"="Affirmed", "Reversed"="Reversed", "Denied"="Denied", "Vacated"="Vacated", "Other"="Other")
    updateRadioButtons(session, "caseType", choices=c(caseType, "All"), selected="", inline=T)
    updateRadioButtons(session, "outcomeType", choices=c(outcomeType, "All"), selected="", inline=T)

    # Construct list of key words, either to be omitted from or identified in text
    pronouns <- tolower(scan("..\\..\\Pronouns.csv", what="character"))
    omitWords <- tolower(scan("..\\..\\OmitWords.csv", what="character"))
    latinLegal <- tolower(xmlToDataFrame("..\\..\\LatinLegalTerms.xml")[,1])

  }
)
