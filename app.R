library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
#read data
investment<-read.csv("investment.csv")
investment$date<-as.Date(investment$date)

ui <- fluidPage(
    mainPanel(
        headerPanel("Final proejct investment decision"),
        tabsetPanel(
            tabPanel("Selected Stock Price",
                     br(),
                     sidebarPanel(
                         selectInput('investmentinput','Investment Type',
                                     choices=sort(unique(investment$investment.type)))
                     ),
                     mainPanel(
                         h3('Stock price'),
                         dataTableOutput('table_investment'),
                     )),
            tabPanel("Investment tracking",
                     br(),
                     sidebarPanel(
                       selectInput("investinput","Investment Type",choices=sort(unique(investment$investment.type)))
                     ),
                     mainPanel(
                       h3('Investment tracking for each stock'),
                       plotOutput('plot_investment'),
                     ))
        )
    )
)

# Define server logic required to draw plot and table
server <- function(input, output) {
  output$table_investment<-renderDataTable({
    investment_each<-investment%>%filter(investment.type==input$investmentinput)
    print(investment_each)
})
  output$plot_investment<-renderPlot({
    plot_investment<-investment%>%filter(investment.type==input$investinput)
    plot<-ggplot(data=plot_investment)+
      geom_line(aes(x=date,y=investment))+
      scale_x_date(breaks = date_breaks("months"))+
      labs(title="Investment tracking",y="Investment Amount")
    plot
})

}
  

# Run the application 
shinyApp(ui = ui, server = server)
