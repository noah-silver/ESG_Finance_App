
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("ESG Rankings on Financial Outcomes"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "xaxis",
                        label = "X Axis Variable",
                        choices = c("Rank","Company","State","Industry","Score","Environmental.Score","Social.Score","Corporate.Governance","HTICK","date","SICCD","PRC","RET","SHROUT","sprtrn","ind","fyear","at","revt","prc_lag","prc_chg","prc_chg_percent","mkt_cap","abnormal_ret","car_1","car_3","rank_grp"),
                        multiple = F,
                        selected = NULL),
            selectInput(inputId = "yaxis",
                        label = "Y Axis Variable",
                        choices = c("Rank","Company","State","Industry","Score","Environmental.Score","Social.Score","Corporate.Governance","HTICK","date","SICCD","PRC","RET","SHROUT","sprtrn","ind","fyear","at","revt","prc_lag","prc_chg","prc_chg_percent","mkt_cap","abnormal_ret","car_1","car_3","rank_grp"),
                        multiple = F,
                        selected = NULL),
            selectInput(inputId = "size",
                        label = "Size Variable",
                        choices = c("Rank","Company","State","Industry","Score","Environmental.Score","Social.Score","Corporate.Governance","HTICK","date","SICCD","PRC","RET","SHROUT","sprtrn","ind","fyear","at","revt","prc_lag","prc_chg","prc_chg_percent","mkt_cap","abnormal_ret","car_1","car_3","rank_grp",NA),
                        multiple = F,
                        selected = NULL),
            selectInput(inputId = "color",
                        label = "Color/Fill Variable",
                        choices = c("Rank","Company","State","Industry","Score","Environmental.Score","Social.Score","Corporate.Governance","HTICK","date","SICCD","PRC","RET","SHROUT","sprtrn","ind","fyear","at","revt","prc_lag","prc_chg","prc_chg_percent","mkt_cap","abnormal_ret","car_1","car_3","rank_grp",NA),
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
library(ggplot2)
library(lubridate)
library(dplyr)
    
    data = read.csv("merged_data.csv")
    
    ##############################################
    #### dates:
    ##############################################
    data$date = ymd(data$date)
    
    ##############################################
    #### construct add'l needed variables:
    ##############################################
    
    ##price changes:
    
    #splitting the monthly data from the daily, getting the lag variable and calculating the:
    #price change
    #price change as a percent
    data = data %>% 
        group_by(HTICK, ind) %>% 
        mutate(prc_lag = lag(PRC), prc_chg = PRC - prc_lag, prc_chg_percent = ((prc_chg/prc_lag)*100)) 
    
    #market capitalization
    #this is one of our control variables
    #it is calculated as shares outstanding X shareprice
    # we want to have ONE value of market cap for the ticker, like how we have one value of assets and revenue
    #so I am going to do avg shares outstanding*avg price
    
    data = data %>%
        group_by(HTICK) %>% 
        mutate(mkt_cap = (mean(SHROUT)*mean(PRC)))
    
    #abnormal returns
    #this can be done in a number of ways, but one simple way
    #is to just take the difference between the firm's return and the SP 500 return
    
    data = data %>% 
        mutate(abnormal_ret = (RET - sprtrn))
    
    #3-day cumulative abnormal ret
    data = data %>%
        group_by(HTICK, ind) %>% 
        mutate(car_1 = (1+abnormal_ret), 
               car_1_lead = lead(car_1), 
               car_1_lead2= lead(car_1_lead), 
               car_3 = (car_1*car_1_lead*car_1_lead2)) %>% 
        select(c(1:25, 28))
    
    #constructing an indicator variable for ranking levels
    data$rank_grp = ifelse(data$Rank <= 50, 1, 
                           ifelse(data$Rank <= 100, 2,
                                  ifelse(data$Rank <= 150, 3,
                                         ifelse(data$Rank <= 200, 4,
                                                ifelse(data$Rank <= 250, 5, 6)))))
    output$plot <- renderPlot({
        
        data%>%
            ggplot(aes_string(x = input$xaxis, y = input$yaxis, size = input$size, color = input$color))+
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
