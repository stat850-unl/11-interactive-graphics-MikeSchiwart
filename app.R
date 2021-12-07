library(plotly)
library(shiny)
library(tidyverse)
library(stringr)
cocktail <- readr::read_csv('cocktail.csv')
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Cocktail Finder"),
    
    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            textInput("text", label = h3("Enter ingredients here"), value = "Bailey's irish cream"),
            hr(),
            fluidRow(column(3, verbatimTextOutput("value")))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("bar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    cocktaild = reactive({
        phrase = input$text
        return(cocktail %>% 
                   filter(agrepl(phrase, merged) == TRUE))
    })
    
    
    output$bar <- renderPlotly({
        cocktail3 <- cocktaild()
        
        p <-  ggplot(data=cocktail3, aes(x=drink, y=ingredients, label = merged))+
            geom_bar(stat="identity")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        ggplotly(p)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)