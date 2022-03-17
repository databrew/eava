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
babel <- read_csv("babel.csv")
# NEED TO UPDATE!
conditions <- c("injury", "fever", "rash", "diarrhea")
mapper_formats <- levels( as.factor( babel$format ))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    # titlePanel("babel rolodex"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        #sidebarPanel(
        column(width=2,
            selectInput("condition","Choose a condition", choices = conditions)
        ),
        column(width=2,
            selectInput("mapper_format", "Choose a mapper format", choices = mapper_formats)
        )
    ),

        # Show a plot of the generated table
    fluidRow(
        column(width=1,
            tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     
    # })
    output$table <- renderTable({
        fever <- babel[ grep(input$condition,babel$question_full), ]
        fever <- fever %>% filter(format==input$mapper_format)
        # knitr::kable(fever)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
