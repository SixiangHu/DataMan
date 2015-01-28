#' sankeyPlot
#'
#' @description Using `rCharts` package and java D3.js sankey plot to visualise rpart decision tree model.  
#' @usage sankeyPlot(model)
#' @param model A rpart decision tree object.
#' @param shiny logical. If `TRUE`, function returns rChart object. Use `showOutput` in `UI` to display.
#' @param domain A character. When `shiny` is `TRUE`, this domain has to be set the same with shiny output variable name.
#' @details D3 java graph library provides lots of interactive visualisation function. 
#' And `rCharts` package provides a link between R and D3. 
#' Hence, this code comes out to plot a decision tree object using  sankey.
#' @author Sixiang Hu
#' @seealso rChart, 
#' @export sankeyPlot

sankeyPlot <- function(model,
                             nodeWidth = 15, 
                             nodePadding = 10, 
                             layout = 32, 
                             width = 600, 
                             height = 600, 
                             units= "%",
                             title=NULL,
                             shiny=FALSE,domain="SankeyPlot"){ 

  fr <- model$frame 
  rn <- row.names(fr) 
  num_row <- length(rn) 

  notename <- paste(as.character(fr$var)," (",as.integer(fr$yval2[(num_row+1):(num_row*2)]),"/",as.integer(fr$yval2[(num_row*2+1):(num_row*3)]),")",sep="") 
  name <- data.frame(from=as.numeric(rn),source=notename) 
  
  total <- fr[1,2] 
  sankeydata <- data.frame(from=as.numeric(rn) %/% 2,to=as.numeric(rn),value=fr$n /total) 
  
  sankeydata1 <- merge(x = sankeydata, y = name, by = "from", all.x=TRUE) 
  
  colnames(name) <-c("to","target") 
  sankeydata <- merge(x = sankeydata1, y = name, by = "to", all.x=TRUE) 
  sankeydata$from <-NULL 
  sankeydata$to <-NULL 
  sankeydata <- sankeydata[,c(2,3,1)] 
  
  
  sankeydata$source <- as.character(sankeydata$source) 
  sankeydata$target <- as.character(sankeydata$target) 
  sankeydata[1,1] <-as.character("Root") 
  
  sankeyPlot <- rCharts$new() 
  sankeyPlot$setLib('inst/libraries/d3_sankey')
  sankeyPlot$setTemplate('inst/libraries/d3_sankey/layouts/chart.html')
  
  sankeyPlot$set( 
    data = sankeydata, 
    nodeWidth = nodeWidth, 
    nodePadding = nodePadding, 
    layout = layout, 
    width = width, 
    height = height, 
    units= units,
    title=title
  )
  
  if (shiny) {
    sankeyPlot$addParams(dom = domain)
    return(sankeyPlot)
  }
  else sankeyPlot 
} 