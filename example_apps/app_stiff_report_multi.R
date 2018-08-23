server <- function(input, output) {
  # Output Gray Histogram
  output$distPlot <- renderPlot({
    write(paste0("hist(rnorm(",input$obs,"), col = 'darkgray', border = 'white') evaluated"),file="app.log",append=TRUE)
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
  # Output Blue Histogram
  output$distPlot2 <- renderPlot({
    write(paste0("hist(rnorm(",input$obs,"), col = 'blue', border = 'white') evaluated"),file="app.log",append=TRUE)
    hist(rnorm(input$obs), col = 'blue', border = 'white')
  })
  # Output Blue Histogram
  output$scatterplot <- renderPlot({
    write(
      paste0("plot(sample(",input$obs,"), sample(",input$obs,") evaluated"),file="app.log",append=TRUE)
    plot(sample(input$obs), sample(input$obs))
  })
  
  # Observe PDF button and create PDF
  observeEvent(input$"renderPDF",{
    tryCatch({
      pdf("test.pdf")
      hist(rnorm(input$obs), col = 'blue', border = 'white')
      hist(rnorm(input$obs), col = 'darkgray', border = 'white')
      plot(sample(input$obs), sample(input$obs))
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
      plotOutput("distPlot2"),
      plotOutput("scatterplot")
    )
  )
)
shinyApp(ui = ui, server = server)
