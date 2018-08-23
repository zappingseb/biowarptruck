library(methods)
library(rlang)


setGeneric("plotElement",where = parent.frame(),def = function(object){standardGeneric("plotElement")})
setGeneric("shinyElement",where = parent.frame(),def = function(object){standardGeneric("shinyElement")})
setClass("AnyPlot", representation(plot_element = "call"))
setClass("HistPlot", representation(color="character",obs="numeric"), contains = "AnyPlot")

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



server <- function(input, output, session) {
  
  # Create a reactive to create the Report object
  report_obj <- reactive(HistPlot(obs=input$obs))
  
  # Check for change of the slider to change the plots
  observeEvent(input$obs,{
    output$renderedPDF <- renderText("")
    output$renderPlot <-  shinyElement(  report_obj() )
  } )
  
}

# Simple shiny App containing the standard histogram + PDF render and Download button
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(
      uiOutput("renderPlot")
    )
  )
)
shinyApp(ui = ui, server = server)