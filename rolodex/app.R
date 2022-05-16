#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

babel <- read_csv("babel.csv") %>%
    select(-response_standarized)
mapper_formats <- levels( as.factor( babel$format ))

# configure hierarchical algorithm (e.g., based on Kalter et al.)
# the code below assumes that:
# (a) name of resulting list is conditions, (b) names(conditions) are the causes, (c) conditions[[cause]] are relevant conditions

# source("kalter_child_config.R")
source("kalter_neonate_config.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("babel rolodex"),

    fluidRow(
        column(width=2,
               selectInput("cause","Variables related to", choices = names(conditions))
        ),
        column(width=2,
               uiOutput("conditionSelection")
        ),
        column(width=2,
            selectInput("mapper_format", "Mapper format", choices = mapper_formats)
        )
    ),

    fluidRow(
        column(width=12,
            tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$conditionSelection <- renderUI({
        selectInput("condition","Relevant conditions", choices = sort(conditions[[input$cause]]))
    })

    output$table <- renderTable({
        babel[ grep(input$condition, babel$question_full, fixed=TRUE), ] %>% filter(format==input$mapper_format)
    }, striped = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
