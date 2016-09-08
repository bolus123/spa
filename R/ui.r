library(shiny)
library(shinythemes)

# Define UI for Statistical Performance Analysis (spa)
shinyUI(fluidPage(
  # Application theme
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
      
      numericInput("sub", "Number of Subgroups (m):", 30),
      
      numericInput("obs", "Subgroup Size (n):", 5),
      
      sliderInput("l", "L:", 
                  min = 2, max = 4, value = 3, step = 0.01),
      
      numericInput("delta", "Shift:", 10)
    ),
    

    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Performance", tableOutput("psum"), tableOutput("qsum")), 
                  tabPanel("PDF-CDF", plotOutput("cdf"), plotOutput("pdf"))
      )
    )
  )
))

