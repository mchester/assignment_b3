library(datateachr)
library(tidyverse)
library(dplyr)
library(shiny)
library(plotly)

options(shiny.autoreload = TRUE)

# define UI

ui <- fluidPage(
  
  # adding application title and summary to help users understand app
  titlePanel("Cancer Sample Visualization Tool"),
  tags$br(),
  
  "This app helps you explore the datateachr (cancer_sample) 
     data set. Select a variable and visualize the difference between
     benign and malignant diagnoses. To find out if differences are
     significant, click the ANOVA and regression tabs. Click on
     the plots or their points to learn more.",
  
  tags$br(),
  tags$br(),
  
  sidebarLayout(
    
    # Adding sidebar with select input to help users cleanly and 
    #effortlessly visualize data
    
    sidebarPanel(
      selectInput("variable","Select Variable",
                  choices = names(cancer_sample %>%
                                    select(-c(ID, diagnosis))))
    ),
    
    # Adding a tabs for plot display, ANOVA 
    # and regression to help users quickly determine significance
    # and adding plotly for more information and interaction
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("boxplot")),
        tabPanel("ANOVA", verbatimTextOutput("summary")),
        tabPanel("Regression", verbatimTextOutput("regression"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  filtered <- reactive(cancer_sample)
  
  output$boxplot <- renderPlotly({
    ggplot(cancer_sample, aes_string(y = input$variable)) +
      geom_boxplot(aes(diagnosis, get(input$variable))) + 
      scale_x_discrete(limits = c("B", "M"),
                       labels = c("benign", "malignant"))
  })
  
  output$summary <- renderPrint({
    model <- aov(get(input$variable) ~ cancer_sample$diagnosis, data = cancer_sample)
    print(summary(model))
  })
  
  output$regression <- renderPrint({
    regression <- lm(get(input$variable) ~ cancer_sample$diagnosis, data = cancer_sample)
    print(summary(regression))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)