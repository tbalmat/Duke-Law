#####################################################################################################
# Duke University Law School
# LexisNexis Data Review App, June 2020
# Shiny user interface
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetw ork/visnetwork.pdf

library(shiny)
library(shinythemes)
library(DT)

# Dir location
setwd("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\MySQL\\CaseDataReviewApp")

shinyUI(
  fluidPage(

    includeCSS("style.css"),
    title="Duke LN Review",

    # Use a div to provide a slight left margin
    div(
      HTML("<h3>Duke University Law - LexisNexis Data Review</h3><br><br>"),
      style="margin-left: 20px"
    ),

    fluidRow(

      # Prompts
      column(width=2,

        # Database password prompt
        div(
          fluidRow(
            column(width=12,
              sidebarPanel(width="100%",
                fluidRow(width=12,
                  div(passwordInput("dbPW", "DB password"), style="width:50%; margin-left:15px; display:inline-block"),
                  div(actionButton("dbConnect", "connect"), style="margin-left:5px; display:inline-block")
                ),
                htmlOutput("dbMsg")
              )
            )
          ),
          style="margin-left: 20px"
        ),

        # Case query prompt
        div(
          fluidRow(
            column(width=12,
              sidebarPanel(width="100%",
                HTML("<b>Case query parameters<br><br><b>"),
                textInput("queryLNI", "LNI"),
                selectInput("queryCourt", "Court", choices=NULL),
                dateInput("queryDate0", "initial date (YYYY-MM-DD)", value=""),
                dateInput("queryDate1", "ending date (YYYY-MM-DD)", value=""),
                textInput("queryTitleText", "title contains text"),
                textInput("queryOutcomeText", "outcome contains text"),
                radioButtons("queryPubStatus", "pub status", choices=c("all"="all", "rep"="reported", "unrep"="unreported"), inline=T),
                radioButtons("queryPerCuriam", "per curiam", choices=c("all", "yes", "no"), inline=T),
                HTML("<br>"),
                actionButton("query", "query"),
                HTML("<br><br>"),
                htmlOutput("queryMsg")
              )
            )
          ),
          style="margin-left: 20px"
        )

      ),

      # Query results
      column(width=10,

        # Case headers
        DT::dataTableOutput("tableCaseHeader", width="95%"),

        HTML("<br><br>"),

        fluidRow(
          column(width=2, htmlOutput("extData")),
          column(width=3, tableOutput("tableLegalTopics")),
          column(width=5, tableOutput("tableCitations"))
        )

      )

    )

  )
)