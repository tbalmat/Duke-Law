# Duke University Law Appeal Text Analysis

# Shiny user interface module

library(shiny)

shinyUI(
  fluidPage(

    HTML("<br><b>Duke University Law School - LexisNexis Appeal Outcome Text Analysis - Word Proportion X-Y Plots</b><br><br>"),

    # prompts
    fluidRow(width=12,
      column(width=2, selectInput("caseType1", "Case Type 1", choices="")),
      column(width=2, selectInput("caseType2", "Case Type 2", choices="")),
      div(column(width=2,
                 actionButton("actionViewPlot1", "View", width="60px", style="margin-top: 25px; padding:6.5px;"),
                 actionButton("actionSavePlot1", "Save", width="60px", style="margin-top: 25px; padding:6.5px;"),
          style="display: inline-block; vertical-align: top; margin-left: 0px")), 
      column(width=2, selectInput("outcomeType1", "Outcome Type 1", choices="")),
      column(width=2, selectInput("outcomeType2", "Outcome Type 2", choices="")),
      div(column(width=2,
                 actionButton("actionViewPlot2", "View", width="60px", style="margin-top: 25px; padding:6.5px;"),
                 actionButton("actionSavePlot2", "Save", width="60px", style="margin-top: 25px; padding:6.5px;"),
          style="display: inline-block; vertical-align: top; margin-left: 0px"))
    ),
    fluidRow(width=12,
      column(width=4, sliderInput("pRange", "p-Window", value=c(0.01, 0.25), min=0, max=0.25, step=0.001, width="90%")),
      div(column(width=6,
                 textInput("graphDir", "Graph Output Directory", width="100%",
                 value=("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\ModelsAnalysis\\OutcomeTextAnalysis\\Review\\images"))),
                 style="display: width: 100%; inline-block; vertical-align: top; margin-top: 10px")
    ),

    # Message line
    fluidRow(width=12, column(width=12, textOutput("msg"))),

    HTML("<br>"),

    # Graph
    fluidRow(width=12,
      column(width=12,
             HTML("<center>"),
             plotOutput("wordProportionXY", width="800px", height="800px"),
             HTML("</center>"))
    )

  )
)
