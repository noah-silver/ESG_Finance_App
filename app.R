
library(shiny)
library(dplyr)
library(ggplot2)
score_cap_return <- data %>%
    filter(ind == "d", date == "2019-11-21", is.na(abnormal_ret) == F)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("ESG Rankings on Financial Outcomes"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "xaxis",
                        label = "X Axis Variable",
                        choices = colnames(score_cap_return),
                        multiple = F,
                        selected = NULL),
            selectInput(inputId = "yaxis",
                        label = "Y Axis Variable",
                        choices = colnames(score_cap_return),
                        multiple = F,
                        selected = NULL),
            selectInput(inputId = "size",
                        label = "Size Variable",
                        choices = c(colnames(score_cap_return),NA),
                        multiple = F,
                        selected = NULL),
            selectInput(inputId = "color",
                        label = "Color/Fill Variable",
                        choices = c(colnames(score_cap_return),NA),
                        multiple = F,
                        selected = NULL),
            textInput(inputId = "title",
                      label = "Title"),
            textInput(inputId = "subtitle",
                      label = "Subtitle"),
            textInput(inputId = "xlab",
                      label = "X Axis Label"),
            textInput(inputId = "ylab",
                      label = "Y Axis Label"),
            textInput(inputId = "slab",
                      label = "Size Label"),
            textInput(inputId = "clab",
                      label = "Color Label")
        ),

        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderPlot({
    ggplot(score_cap_return, aes_string(x = input$xaxis, y = input$yaxis, size = input$size, color = input$color))+
            geom_point()+
            labs(x=input$xlab,
                 y=input$ylab, 
                 title=input$title,
                 subtitle = input$subtitle, 
                 size = input$slab,
                 color = input$clab)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
