library(methods)
library(stringr)
library(rlang)
library(glue)

setGeneric("plotElement",where = parent.frame(),def = function(object){standardGeneric("plotElement")})
setGeneric("pdfElement",where = parent.frame(),def = function(object){standardGeneric("pdfElement")})
setGeneric("shinyElement",where = parent.frame(),def = function(object){standardGeneric("shinyElement")})
setGeneric("logElement",where = parent.frame(),def = function(object){standardGeneric("logElement")})

setClass("AnyPlot", representation(plot_element = "call"))
setClass("HistPlot", representation(color="character",obs="numeric"), contains = "AnyPlot")
setClass("ScatterPlot", representation(obs="numeric"), contains = "AnyPlot")
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
ScatterPlot <- function(obs=100){
  new("ScatterPlot",
      plot_element = expr(plot(sample(!!obs),sample(!!obs))),
      obs = obs
  )
}

Report <- function(obs=100){
  new("Report",
    plots = list(
      HistPlot(color="darkgrey", obs=obs),
      HistPlot(color="blue",     obs=obs),
      ScatterPlot(obs=obs)
    ),
    filename="test.pdf",
    obs=obs,
    rendered=FALSE
  )
}


#' Method to log a Plot Element
setMethod("logElement",signature = "AnyPlot",definition = function(object){
 # print(deparse(object@plot_element))
  write(paste0(deparse(object@plot_element)," evaluated"),file="app.log",append=TRUE)
})
#' Method to plot a Plot element
setMethod("plotElement",signature = "AnyPlot",definition = function(object){
  eval(object@plot_element)
})
#' Method to render a Plot Element
setMethod("shinyElement",signature = "AnyPlot",definition = function(object){
  renderPlot(plotElement(object))
})

#' Method to generate a PDF Report from a Report Element
setMethod("pdfElement",signature = "Report",definition = function(object){
  tryCatch({
    pdf(object@filename)
    lapply(object@plots,function(x){
      plotElement(x)
    })
    dev.off()
    object@rendered <- TRUE
  },error=function(e){warning("plot not rendered")#do nothing
    })
  return(object)
})

#' Tell how to generate the shiny Element for a report
setMethod("shinyElement",signature = "Report",definition = function(object){
  renderUI({
    lapply(object@plots,
            function(x){
              logElement(x)
              shinyElement(x)
      })
  })
})


server <- function(input, output, session) {
  
  # Create a reactive to create the Report object
  report_obj <- reactive(Report(obs=input$obs))
  
  # Check for change of the slider to change the plots
  observeEvent(input$obs,{
    output$renderedPDF <- renderText("")
    output$reportReport <-  shinyElement(  report_obj() )
  } )
  
  # Observe PDF button and create PDF
  observeEvent(input$"renderPDF",{
    report <- pdfElement(report_obj())
    if(report@rendered){
      output$renderedPDF <- renderText("PDF rendered")
    }else{
      output$renderedPDF <- renderText("PDF could not be rendered")
    }
  })
  
  # Observe Download Button and return rendered PDF
  output$downloadPDF <- downloadHandler(
    filename = report_obj()@filename,
    content = function(file) {
      file.copy(report_obj()@filename, file, overwrite = TRUE)
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
      uiOutput("reportReport")
    )
  )
)
shinyApp(ui = ui, server = server)