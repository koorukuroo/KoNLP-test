library(shiny)

shinyUI(fluidPage(
  titlePanel("Shiny Application"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "start",
                   label = "input start year : ",
                value = "1893"
                ), # start text input
      textInput(inputId = "end",
                label = "input end year : ",
                value = "2005"
                ), # end text input
      sliderInput(inputId = "alphaSlider",
                label = "alpha",
                min = 0,
                max = 1,
                value = 1,
                step = 0.1
                ), # slider input
      checkboxInput(inputId = "smooth",
                label = "With smooth ?",
                value = FALSE
                ), # checkbox input
      
      submitButton("Update View"), # submit button
      hr(),
      helpText("made by Joshua")
      ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", dataTableOutput("table"))
                  )
      )
    )
))
