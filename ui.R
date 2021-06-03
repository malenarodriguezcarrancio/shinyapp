library(shiny)
library(ggplot2)
library(treemap)
library(htmlwidgets)
library(data.tree)
library(dplyr)
library(viridis)

#devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)

dataset <- get(load(url("https://github.com/pedroconcejero/master_UNED/blob/master/datos_4510_vehiculos_2016.rda?raw=true")))

nums <- sapply(dataset, is.numeric)
continuas <- names(dataset)[nums]
cats <- sapply(dataset, is.character)
categoricas <- names(dataset)[cats]



dataset$Expensive <- if_else(dataset$Manufacturer=='Rolls Royce'| dataset$Manufacturer=='LAMBORGHINI'| dataset$Manufacturer=='FERRARI' | dataset$Manufacturer=='ASTON MARTIN LAGONDA'| dataset$Manufacturer=='BENTLEY MOTORS',dataset$Manufacturer, 'Other')
dataset$Cheap <- if_else(dataset$Manufacturer=='SMART'| dataset$Manufacturer=='DS'| dataset$Manufacturer=='VOLVO' | dataset$Manufacturer=='SKODA'| dataset$Manufacturer=='FIAT',dataset$Manufacturer, 'Other')
brands <- names(dataset[,c(29,30)])
fuels <- names(dataset[, c(6,28)])
emisiones <- names(dataset[,c(17,23:25,27)])




## SHINY ##
shinyUI(
  navbarPage("Shiny Visualización Avanzada",
             tabPanel("Scatterplot consumo y más",
                      sidebarPanel(
                        
                        selectInput('x', 'Elige variable para eje X', continuas, continuas[[1]]),
                        selectInput('y', 'Elige variable para eje Y', continuas, continuas[[8]]),
                        selectInput('color', 'Color', c('None', 'Tipo')),
                        
                        checkboxInput('lm', 'Línea de Regresión'),
                        checkboxInput('smooth', 'Suavizado LOESS'),
                        
                        selectInput('facet_row', 'Elige variable para facetas por filas', c(None='.', categoricas))
                      ),
                      
                      mainPanel(
                        plotOutput('plot',
                                   height=500)
                        
                      )
             ),
                tabPanel("Diagrama de Árbol",
                          sidebarPanel(
                              selectInput('Group0', 'Elige variable para grupo', categoricas, categoricas[[1]]),
                              selectInput('Group1', 'Elige variable para subgrupo', categoricas, categoricas[[5]]),
                              selectInput('Group2', 'Elige variable para subgrupo 2', categoricas, categoricas[[2]]),
                              selectInput('y2', 'Elige variable para métrica', continuas, continuas[[1]]),
                         ),
                         mainPanel(
                           d3tree2Output('plot2',
                                        height=500)
                            
                         )
                 ),
                
                tabPanel("Gráfico de Violín",
                         sidebarPanel(
                          selectInput('x3', 'Elige', brands, brands[[2]]),
                          selectInput('Fill', 'Elige', fuels, fuels[[2]]),
                          selectInput('y3', 'Elige variable para eje Y', emisiones, emisiones[[2]])
                         ),
                         
                         mainPanel(
                           plotOutput('plot3',
                                         height=500)
                           
                         )
                         
                )
               
    ))
