# Define Generics ---------------------------------------------------------------------------------
setGeneric("evalElement",where = parent.frame(),def = function(object){standardGeneric("evalElement")})
setGeneric("pdfElement",where = parent.frame(),def = function(object){standardGeneric("pdfElement")})
setGeneric("shinyElement",where = parent.frame(),def = function(object){standardGeneric("shinyElement")})
setGeneric("logElement",where = parent.frame(),def = function(object){standardGeneric("logElement")})

# Define Report and Plot Classes --------------------------------------------------------------------


#' Report Module for apps
#' 
#' @slot plots (\code{list}) A list of \link{AnyPlot} objects
#' @slot filename (\code{character}) The name of the PDF to output
#' @slot obs (\code{numeric}) The number of observations to be used to generate the report
#' @slot rendered  (\code{logical}) Whether the PDF was rendered or not
#' 
#' @export
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})  
setClass("Report",
         representation(
           plots="list",
           filename="character",
           obs="numeric",
           rendered="logical"))

# Report Methods ------------------------------------------------------------

#' Method to generate a PDF Report from a Report Element
#' 
#' This function generates a PDF with all elements of the report
#' and stores it on the drive. The PDF will have the name of the
#' \code{filename} slot of the object.
#' 
#' @param object \code{Report} An object
#' 
#' @return object \code{Report} with a changed \code{rendered} slot
#' @export
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
setMethod("pdfElement",signature = "Report",definition = function(object){
  tryCatch({
    pdf(object@filename)
    lapply(object@plots,function(x){
      pdfElement(x)
    })
    dev.off()
    object@rendered <- TRUE
  },error=function(e){warning("plot not rendered")#do nothing
  })
  return(object)
})

#' Method to generate the shiny Element for a Report
#' 
#' Log the call of each object inside the \code{plots} slot and additionally render
#' it into the shiny output
#' 
#' @param object \code{Report} An object
#' 
#' @return The output of a \link[shiny]{renderUI} output of shiny
#' 
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
#' @export
setMethod("shinyElement",signature = "Report",definition = function(object){
  renderUI({
    lapply(object@plots,
           function(x){
             logElement(x)
             shinyElement(x)
           })
  })
})

#- Anyplot class ----------------------------------------------------------------

#' Plot Module for apps
#' 
#' @slot plot_element (\code{call}) A call that can upon \code{eval} return a plot element
#' 
#' @export
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})  


# Constructors of AnyPlot classes ---------------------------------------------------------------------------

#' Construct the AnyPlot class
#' @param plot_element (\code{call}) A call returning a plot
#' @export
#' @return \code{AnyPlot} an object of class AnyPlot
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
AnyPlot <- function(plot_element=expr(plot(1,1))){
  new("AnyPlot",
      plot_element = plot_element
  )
}

# Plot Methods ------------------------------------------------------------

#' Method to log a Plot Element
#' 
#' @param object \code{AnyPlot} An object
#' 
#' @return nothing is returned, the call of the object is written into a logfile
#' @export
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
setMethod("logElement",signature = "AnyPlot",definition = function(object){
  write(paste0(deparse(object@plot_element)," evaluated"), file="app.log",append=TRUE)
})

#' Method to plot a Plot element
#' 
#' @param object \code{AnyPlot} An object
#' 
#' @return A plot object, fully rendered
#' @export
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
setMethod("evalElement",signature = "AnyPlot",definition = function(object){
  eval(object@plot_element)
})
#' Method to return a PDF element
#' @param object \code{AnyPlot} An object
#' 
#' @return Same call as \link{evalElement}
#' @export
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
setMethod("pdfElement",signature = "AnyPlot",definition = function(object){
  evalElement(object)
})

#' Method to shiny output a Plot
#' 
#' @param object \code{AnyPlot} An object
#' 
#' @return A Shiny element created by \link[shiny]{renderPlot}
#' 
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
#' @export
setMethod("shinyElement",signature = "AnyPlot",definition = function(object){
  renderPlot(evalElement(object))
})

