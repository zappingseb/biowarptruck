library(methods)
library(stringr)
library(rlang)
library(glue)

setGeneric("plotElement",where = parent.frame(),def = function(object){standardGeneric("plotElement")})
setGeneric("pdfElement",where = parent.frame(),def = function(object){standardGeneric("pdfElement")})
setGeneric("shinyElement",where = parent.frame(),def = function(object){standardGeneric("shinyElement")})

setClass("AnyPlot", representation(plot_element = "call"))
setClass("HistPlot", representation(color="character",obs="numeric"), contains = "AnyPlot")
setClass("Report",representation(plots="list",filename="character",obs="numeric",rendered="logical"))

AnyPlot <- function(plot_element=expr(plot(1,1))){
  new("AnyPlot",
      plot_element = plot_element
  )
}

HistPlot <- function(color="darkgrey",obs=100){
  new("HistPlot",
      plot_element = expr(hist(rnorm(!!obs), col = !!color, border = 'white')),
      color = color,
      obs = obs
      )
}

#' Method to plot a Plot element
setMethod("plotElement",signature = "AnyPlot",definition = function(object){
  eval(object@plot_element)
})
#' Method to render a Plot Element
setMethod("shinyElement",signature = "AnyPlot",definition = function(object){
  renderPlot(plotElement(object))
})

#' Method to generate a PDF Report from a Report Element
setMethod("pdfElement",signature = "AnyPlot",definition = function(object){
    pdf("test.pdf")
    plotElement(object)
    dev.off()
})


server <- function(input, output) {
  
  # Create a reactive to create the HistPlot object
  report_obj <- reactive(HistPlot(obs=input$obs))
  
  # Check for change of the slider to change the plots
  observeEvent(input$obs,{
    output$renderedPDF <- renderText("")
    output$reportReport <-  shinyElement(  report_obj() )
  } )
  
  # Observe PDF button and create PDF
  observeEvent(input$"renderPDF",{
    report <- pdfElement(report_obj())
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
      plotOutput("reportReport")
    )
  )
)
shinyApp(ui = ui, server = server)