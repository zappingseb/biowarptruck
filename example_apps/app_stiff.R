server <- function(input, output) {
  # Output Gray Histogram
  output$distPlot <- renderPlot({
    x = sample(x = seq(from=0,to=100,by=0.1), size=input$obs,replace=T)
    plot(
      x = x,
      x+sample(x = 0.1:4, size=input$obs,replace=T)*sample(x = c(-1,1), size=input$obs,replace=T)
    )
  })

}

# Simple shiny App containing the standard histogram + PDF render and Download button
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "obs",
        "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
shinyApp(ui = ui, server = server)
