url<- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv"

mortality_data <- read.csv(url, header = T, stringsAsFactors = F)

df<- mortality_data %>% 
  mutate_at(c('Population', 'Deaths'), as.numeric)

neoplasm2010_df <- df %>% 
  filter(., Year == '2010' & ICD.Chapter == 'Neoplasms') %>% 
  arrange(desc(State), Crude.Rate)

# Define the Shiny UI.
ui <- fluidPage(
  headerPanel('US State Crude Mortality Rates by Cause of Death'),
  sidebarPanel(
    selectInput('Cause', 'Cause of Death', unique(df$ICD.Chapter),
                selected = 'Certain infectious and parasitic diseases',
                width = 'auto')
    
  ),
  mainPanel(
    htmlOutput(outputId = 'results'),
    plotOutput('CDRatePlot', height = 'auto')
  )
)

# Define the Shinny Server.
server <- shinyServer(function(input, output, session) {
  selectedData <- reactive({
    df  %>% filter(ICD.Chapter == input$Cause & Year == 2010)
  })
  
  output$selection <- renderText({
    paste('<i>Crude Mortality Rate For: </i>', input$Cause)
  })
  
  output$CDRatePlot <- renderPlot({
    ggplot(selectedData(), aes(x = reorder(State, -Crude.Rate), y = Crude.Rate)) +
      geom_col(fill = '#8B008B') +
      coord_flip() +
      geom_text(aes(label = Crude.Rate),
                size = 3,
                hjust = -0.2) +
      xlab('State') +
      ylab('Crude Mortality Rate') +
      theme(panel.background = element_blank())
  }, height = function() {
    session$clientData$output_CDRatePlot_width}
  )
})

shinyApp(ui = ui, server = server)