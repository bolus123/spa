library(shiny)
library(shinythemes)

# Define UI for Statistical Performance Analysis (spa)
shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("Statistical Performance Analysis"),

  sidebarLayout(
    sidebarPanel(
      selectInput("cc", "Control Chart:", 
                  choices = c("X-bar Chart")),

      radioButtons("case", "Case:",
                   c("Known-Known" = "kk",
                     "Known-Unknown" = "ku",
                     "Unknown-Known" = "uk",
                     "Unknown-Unknown" = "uu")),
      
      selectInput("est", "Estimator:", 
                  choices = c("s-pooled", "s", "Range")),
      
      numericInput("sub", "Number of Subgroups (m):", 10),
      
      numericInput("obs", "Subgroup Size (n):", 20),
      
      sliderInput("l", 
                  "L:", 
                  value = 3,
                  min = 2, 
                  max = 4),
      
      numericInput("delta", "Shift:", 10)
    ),
    

    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Performance", tableOutput("summary")), 
                  tabPanel("PDF-CDF", verbatimTextOutput("summary"))
      )
    )
  )
))

