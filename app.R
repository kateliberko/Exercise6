library(shiny)
library(tidyverse)
covid19 <- readcsv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")


ui <- fluidPage(h3("Create a Covid19 comparison by choosing one state or more!"),
  selectInput("statename", 
            "Look up state(s): ", 
            unique(as.character(covid19$state)), multiple = TRUE),
  submitButton(text = "Create Covid Graph"),
  plotOutput(outputId = "stateplot")
)

server <- function(input, output) {
  output$stateplot <- renderPlot({
    
    covid19 %>% 
      filter(cases >= 20, state == input$statename) %>% 
      ggplot() +
      geom_line(aes(x = date, y = cases, color = state)) +
      scale_y_log10()+
      theme_minimal()+
      labs(title = "Cumulative Number of COVID cases over time", x= "Cases(over 20)", y="")
  })
}

shinyApp(ui = ui, server = server)