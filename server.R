library(shiny)
library(ggplot2)
library(treemap)
library(d3treeR)
library(htmlwidgets)
library(data.tree)
library(dplyr)
library(viridis)


dataset <- get(load(url("https://github.com/pedroconcejero/master_UNED/blob/master/datos_4510_vehiculos_2016.rda?raw=true")))

dataset$Expensive <- if_else(dataset$Manufacturer=='Rolls Royce'| dataset$Manufacturer=='LAMBORGHINI'| dataset$Manufacturer=='FERRARI' | dataset$Manufacturer=='ASTON MARTIN LAGONDA'| dataset$Manufacturer=='BENTLEY MOTORS',dataset$Manufacturer, 'Other')
dataset$Cheap <- if_else(dataset$Manufacturer=='SMART'| dataset$Manufacturer=='DS'| dataset$Manufacturer=='VOLVO' | dataset$Manufacturer=='SKODA'| dataset$Manufacturer=='FIAT',dataset$Manufacturer, 'Other')
brands <- names(dataset[,c(29,30)])
fuels <- names(dataset[, c(6,28)])
emisiones <- names(dataset[,c(17,23:25,27)])


shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset, 
                aes_string(x=input$x, y=input$y)) + geom_point() 
    
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, "~ .")
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$lm)
      p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T)
    if (input$smooth)
      p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)
    
    print(p)
    
  })
  
  output$plot2 <- renderD3tree2({d3tree2( t <- treemap(dataset,
                                                       index=c(input$Group0,input$Group1,input$Group2),
                                                       vSize= input$y2,
                                                       type="index",
                                                       #fun.aggregate = "mean",
                                                       palette = "Set2",
                                                       bg.labels=c("white"),
                                                       title="TreeMap",            
                                                       fontsize.title=12, 
                                                       align.labels=list(
                                                          c("center", "center"), 
                                                          c("right", "bottom")
                                                       )  
  )            
  
                                          )})
  
  output$plot3 <- renderPlot({
     
     m <- ggplot(dataset, aes_string( y=input$y3, fill=input$Fill, x=input$x3)) + 
                   geom_violin(position="dodge", alpha=0.5) +
                     scale_fill_viridis(discrete=T, name="") +
                     xlab("Brands") +
                     ylab("Emissions") 
                              
  
     print(m)
     
  })

})