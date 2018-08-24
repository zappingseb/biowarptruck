
setClass("TableElement", representation(obs="numeric"), contains = "AnyPlot")


# Define Children of Report class to enable different reports easily ---------------
setClass("TextReport",contains = "Report")




#' Construct the TableElement class
#' 
#' The class constructed will contain a random data.frame with one
#' column and 5 elements as its plot_element
#' 
#' @param obs (\code{numeric}) How many observations to show in the histogram
#' 
#' @return \code{TableElement} an object of class TableElement
#' 
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
TableElement <- function(obs=100){
  new("TableElement",
      plot_element = expr(data.frame(x=sample(x=!!obs,size=5)))
  )
}


#' Constructor for a TextReport
TextReport <- function(obs=100){
  new("TextReport",
      plots=list(
        TableElement(obs=obs)
      ),
      filename="test_text.pdf",
      obs=obs,
      rendered=F
  )
}


# Table Methods -------------------------------------------------------------

setMethod("shinyElement",signature = "TableElement",definition = function(object){
  renderDataTable(plotElement(object))
})

#' Method to plot a Plot element
setMethod("pdfElement",signature = "TableElement",definition = function(object){
  grid.table(plotElement(object))
})
