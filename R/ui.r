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
                   c("KK" = "kk",
                     "KU" = "ku",
                     "UK" = "uk",
                     "UU" = "uu")),
      
      selectInput("est", "Estimator:", 
                  choices = c("s-pooled", "s", "Range")),
      
      numericInput("obs", "Subgroup Size (n):", 5),
      
      numericInput("sub", "Number of Subgroups (m):", 25),
      
      sliderInput("l", "L:", 
                  min = 2, max = 4, value = 3, step = 0.01),
      
      numericInput("t_0", "Target for RL0:", 370),
      
      sliderInput("delta", "Shift:", 
                  min = 0, max = 3, value = 1, step = 0.01),
      
      numericInput("t_d", "Target for RL Delta:", 3)
    ),
    

    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Performance", tableOutput("psum"), tableOutput("qsum")), 
                  tabPanel("PDF-CDF", plotOutput("cdf"), plotOutput("pdf"))
      )
    )
  )
))

