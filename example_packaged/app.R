library(devtools)

# Derive the core package to have all basics inside
devtools::load_all("./core")

server <- function(input,output){
  
  # Derive the infos from the configuration and store it inside a list
  configuration <- xmlApply(xmlRoot(xmlParse("config.xml")),function(xmlItem){
    load_module(xmlItem)
    list(
      name = xmlValue(xmlItem[["name"]]),
      class = xmlValue(xmlItem[["class"]]),
      id = xmlValue(xmlItem[["id"]])
    )
  })
  
  # Append Tabs to the Reporting Window
  xmlApply(xmlRoot(xmlParse("config.xml")),function(xmlItem){
    appendTab("modules",module_tab(xmlItem),select = TRUE)
  })
  
  # Create a reactive to create the Report object due to
  # the chosen module
  report_obj <- reactive({
    module <- unlist(lapply(configuration,function(x)x$name==input$modules))
    if(!any(module))module <- c(TRUE,FALSE)
    do.call(configuration[[which(module)]][["class"]],
            args=list(
              obs = input$obs
    ))
  })
  
  # Check for change of the slider/tab to re-calculate the report modules
  observeEvent({input$obs
    input$modules},{
      
      # Clear all produced outputs
      output$renderedPDF <- renderText("")
      file.remove(list.files(pattern=".pdf"))
      
      # Derive chosen tab
      module <- unlist(lapply(configuration,function(x)x$name==input$modules))
      if(!any(module))module <- c(TRUE,FALSE)
      
      # Re-render the output of the chosen tab
      output[[configuration[[which(module)]][["id"]]]] <- shinyElement(  report_obj() )
    })
  
  # Observe PDF button and create PDF
  observeEvent(input$"renderPDF",{
    
    # Create PDF
    report <- pdfElement(report_obj())
    
    # If the PDF was successfully rendered update text message
    if(report@rendered){
      output$renderedPDF <- renderText("PDF rendered")
    }else{
      output$renderedPDF <- renderText("PDF could not be rendered")
    }
  })
  
  # Observe Download Button and return rendered PDF
  output$downloadPDF <- 
    downloadHandler(
      filename =  report_obj()@filename,
      content = function(file) {
        file.copy( report_obj()@filename, file, overwrite = TRUE)
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
      tabsetPanel(id='modules')  
    )#mainPanel
  )#sidebarlayout
)#fluidPage


shinyApp(ui = ui, server = server)