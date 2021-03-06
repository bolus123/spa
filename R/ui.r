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
                   c("Known Mean - Known Standard Deviation" = "kk",
                     "Known Mean - Unknown Standard Deviation" = "ku",
                     "Unknown Mean - Known Standard Deviation" = "uk",
                     "Unknown Mean - Unknown Standard Deviation" = "uu")),
      
      numericInput("obs", "Subgroup Size (n):", 5),
      
      sliderInput("l", "L:", 
                  min = 2, max = 4, value = 3, step = 0.01),
      
      numericInput("t_0", "Target for In-Control Run Length:", 370),
      
      sliderInput("delta", "Scaled Shift:", 
                  min = 0, max = 3, value = 0.5, step = 0.01),
      
      numericInput("t_d", "Target for Out-of-Control Run Length:", 3)
    ),
    

    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Performance", 
                           fluidRow(
                             column(5,
                                    tableOutput("psum"), plotOutput("box_ic")),
                             column(5,
                                    tableOutput("psum_1"), plotOutput("box_oc"))
                           )
                  ),

                  tabPanel("PDF-CDF", 
                           fluidRow(
                             column(6,
                                plotOutput("cdf"), plotOutput("pdf")),
                             column(6,
                                plotOutput("cdfrl1"), plotOutput("pdfrl1"))
                                    )
                                    )
      )
    )
  )
))

