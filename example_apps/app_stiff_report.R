server <- function(input, output) {
  # Output Gray Histogram
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
  # Observe PDF button and create PDF
  observeEvent(input$"renderPDF",{
      pdf("test.pdf")
      hist(rnorm(input$obs), col = 'darkgray', border = 'white')
      dev.off()
      output$renderedPDF <- renderText("PDF rendered")
    
  })
  
  # Observe Download Button and return rendered PDF
  output$downloadPDF <- downloadHandler(
      filename = "test.pdf",
      content = function(file) {
        file.copy("test.pdf", file, overwrite = TRUE)
      }
    )
}

# Simple shiny App containing the standard histogram + PDF render and Download button
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100),
      actionButton("renderPDF","Render a Report"),
      textOutput("renderedPDF"),
      downloadButton("downloadPDF","Get rendered PDF")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
shinyApp(ui = ui, server = server)
