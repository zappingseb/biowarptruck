server <- function(input, output) {
  # Output Gray Histogram
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
  # Output Blue Histogram
  output$distPlot2 <- renderPlot({
    hist(rnorm(input$obs), col = 'blue', border = 'white')
  })
  
  # Observe PDF button and create PDF
  observeEvent(input$"renderPDF",{
    tryCatch({
      pdf("test.pdf")
      par(mfrow=c(2,1))
      hist(rnorm(input$obs), col = 'blue', border = 'white')
      hist(rnorm(input$obs), col = 'darkgray', border = 'white')
      dev.off()
      output$renderedPDF <- renderText("PDF rendered")
    },error=function(e){output$renderedPDF <- renderText("PDF could not be rendered")})
   
    
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
      plotOutput("distPlot"),
      plotOutput("distPlot2")
    )
  )
)
shinyApp(ui = ui, server = server)
