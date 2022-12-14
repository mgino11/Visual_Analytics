nat_avg <- df %>% 
  group_by(Year, ICD.Chapter) %>% 
  mutate(NationalPopulation = sum(Population),
         NationalCount = sum(Deaths),
         NationalCrude_rate = round(
           100000 * (NationalCount/NationalPopulation), 2)) %>% 
  group_by(Year, ICD.Chapter, State) %>% 
  mutate(Sumcount = sum(Deaths),
         SumCrude_rate = round(100000 * (Sumcount/Population),2)) %>% 
  select(ICD.Chapter, State, Year, NationalCrude_rate, SumCrude_rate)


interface <- fluidPage(
  headerPanel('US State Crude Mortality Rates by Cause of Death Compared To National Average'),
  sidebarPanel(
    selectInput("State", 'State:',
                width = 'auto',
                choices = nat_avg$State, 1
    ),
    selectInput("Cause", 'Cause of Death:',
                width = 'auto',
                choices = nat_avg$ICD.Chapter, 1
    ),
    checkboxInput(inputId = "national_rate", label = strong('Overlay National Crude Mortality Rate Average'), value = FALSE),
    helpText("Select a State and Cause of Death above to see the crude mortality rate."),
    width = "auto"
  ),
  mainPanel(
    htmlOutput(outputId = 'national-results'),
    plotOutput('NationalCDRatePlot', height = 'auto')
  ))
# Define the Shinny Server.
app_two_server <- shinyServer(function(input, output, session) {
  output$NationalCDRatePlot <- renderPlot({
    ggplot(data = 
             nat_avg[nat_avg$State == input$State 
                     & nat_avg$ICD.Chapter == input$Cause,], 
           aes(x = Year, y = SumCrude_rate)) +
      labs(x = 'Year', y = 'Crude Mortality Rate') +  
      geom_bar(stat = 'identity', fill = '#00008B') +
      
      # Conditionally render the National Crude Mortality rate if the Overlay National Crude Mortality Rate Average checkbox is checked.
      if (input$national_rate) {
        geom_line(aes(x = Year, y = NationalCrude_rate), col = '#DC143C', lwd = 1)
      }
    else {
      NULL
    }
  }, height = function() {
    session$clientData$output_NationalCDRatePlot_width}
  )
})

shinyApp(ui=interface, server=app_two_server)