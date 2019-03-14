

# Define Classes to use inside the apps ------------------------------------------------------------
setClass("HistPlot", representation(color="character",obs="numeric"), contains = "AnyPlot")
setClass("ScatterPlot", representation(obs="numeric"), contains = "AnyPlot")
setClass("PlotReport",contains = "Report")


#' Construct the HistPlot class
#' 
#' The class constructed will contain a Histogram as
#' its plot element
#' 
#' @param color (\code{character}) A color in which to show the bars of the plot
#' @param obs (\code{numeric}) How many observations to show in the histogram
#' 
#' @return \code{HistPlot} an object of class HistPlot
#' 
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
HistPlot <- function(color="darkgrey",obs=100){
  new("HistPlot",
      plot_element = expr(hist(rnorm(!!obs), col = !!color, border = 'white')),
      color = color,
      obs = obs
  )
}

#' Construct the ScatterPlot class
#' 
#' The class constructed will contain a random scatterplot as
#' its plot element
#' 
#' @param obs (\code{numeric}) How many observations to show in the histogram
#' 
#' @return \code{ScatterPlot} an object of class ScatterPlot
#' 
#' @author Sebastian Wolf (\email{zappingseb@@gmail.com})
ScatterPlot <- function(obs=100){
  new("ScatterPlot",
      plot_element = expr(plot(sample(!!obs),sample(!!obs))),
      obs = obs
  )
}

#' Constructor of a PlotReport
PlotReport <- function(obs=100){
  new("PlotReport",
      plots = list(
        HistPlot(color="darkgrey", obs=obs),
        ScatterPlot(obs=obs)
      ),
      filename="test_plots.pdf",
      obs=obs,
      rendered=FALSE
  )
}