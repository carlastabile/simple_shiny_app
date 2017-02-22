#
# This is a Shiny web application created for class Advanced Data Visualization
# for the Data Science Master at Universitat de Valencia
# 
# Author: Carla Urrea Stabile
# Last day of modification: 22 feb 2017
#

library(ggplot2)
library(shiny)
library(shinythemes)
library(dplyr)

ui <- navbarPage("App Ciencia de datos",
         theme = shinytheme("flatly"),
         tabPanel("Selección de máquina",
            sidebarLayout(
              sidebarPanel("Maquina",
                           fileInput("archivo","Selecciona un archivo"),
                           uiOutput("selector")
                           
              ),
              mainPanel("Probabilidad de orden", align = "center",
                        plotOutput("gplot")
              )
            )
         ),
         
         
         navbarMenu("Estado de la Máquina",
            tabPanel("Evolución Temporal Alarmas",
               sidebarLayout(
                  sidebarPanel("Alarmas RadioButtons", width = 4, align = "center",
                    uiOutput("radioButtonAlarma")
                  ),
                  mainPanel("Evolución Temporal Alarmas", align = "center", 
                            plotOutput("plotAlarmas")
                  )
               )
            ),
            tabPanel("Registros de la máquina",
               sidebarLayout(
                 sidebarPanel("Alarmas Checkboxes", width = 4, align = "center",
                              uiOutput("checkboxesAlarma")
                 ),
                 mainPanel("Registros de la máquina seleccionada", align = "center",
                           dataTableOutput("tablaMaquina"))
               )
            )
         ),
         tabPanel("Estadísticas Globales Temporales",
                  sidebarLayout(
                    sidebarPanel("PERIODO Y ESTADISTICAS", width = 4, align = "center",
                       uiOutput("periodo"),
                       h5("Histograma"),
                       uiOutput("checkboxAlarmaEstadistica"),
                       sliderInput("binHist","Ancho del bin del histograma",min=1,max=50,value=10),
                       h5("BOXPLOT"),
                       checkboxInput("todas_maquinas","Todas las máquinas")
                    ),
                    mainPanel("Histograma de la alarma seleccionada", align = "center",
                              plotOutput("histograma"),
                              "Boxplot de la alarma seleccionada",
                              plotOutput("boxplot"))
                    
                    
                  )         
        )
         
)




server <- function(input, output){
  
  datos <-reactive({
    fichero <- input$archivo
    if (is.null(fichero)) return(NULL)
    df = eval(parse(text = load(fichero$datapath)))
    return(df) 
  })
  
  output$selector<-renderUI({
    selectInput('TipoSel','Tipo de máquina',datos()$matricula,c(1))
  })
  
  output$gplot <- renderPlot({
    if (is.null(input$archivo)) return(NULL)
    ggplot(datos()[datos()$matricula == input$TipoSel,], aes(y=p_orden, x = dia, color = p_orden))+
      scale_color_continuous(name="p_orden", low="blue",high="red")+
      geom_point()+
      geom_line()
  })
  
  output$radioButtonAlarma <- renderUI({
    if (is.null(input$archivo)) return(NULL)
    names_idx <- grep("^a", names(datos()))
    radioButtons("Alarma","Selecciona la alarma a visualizar",names(datos())[names_idx])
  })
  
  output$plotAlarmas <- renderPlot({
    if (is.null(input$archivo)) return(NULL)
    ggplot(datos()[datos()$matricula == input$TipoSel,], aes(y=get(input$Alarma), x = dia))+
      labs(y=input$Alarma)+
      geom_point()+
      geom_line()
  })
  
  output$checkboxesAlarma <- renderUI({
    if (is.null(input$archivo)) return(NULL)
    names_idx <- grep("^a", names(datos()))
    checkboxGroupInput("Checkbox","Selecciona las alarmas para ver en la tabla",
                       names(datos()[names_idx]),
                       selected = names(datos()[names_idx])[1])
  })
  
  output$tablaMaquina <- renderDataTable({
    if (is.null(input$archivo)) return(NULL)
    datos()[datos()$matricula==input$TipoSel, c("matricula","dia",input$Checkbox,"p_orden")]
  })
  
  output$periodo <- renderUI({
    if (is.null(input$archivo)) return(NULL)
    dateRangeInput("periodo", "Selecciona el periodo",start = "2016-01-01", end = NULL,
                   min = NULL, max = NULL, format = "yyyy-mm-dd", 
                   weekstart = 1, language = "es", separator = "hasta"
    )
  })
  
  output$checkboxAlarmaEstadistica <- renderUI({
    if (is.null(input$archivo)) return(NULL)
    names_idx <- grep("^a", names(datos()))
    selectInput("SelectAlarmaEst","Alarma",
                       names(datos()[names_idx]),
                       selected = names(datos()[names_idx])[1])
    }
  )
  
  output$histograma <- renderPlot({
    if (is.null(input$archivo)) return(NULL)
    range = datos()$dia > input$periodo[1] & datos()$dia < input$periodo[2]
    datasubset <- datos()[range & datos()$matricula == input$TipoSel,]
    ggplot(datasubset, aes_string(input$SelectAlarmaEst)) +
      geom_histogram(binwidth = input$binHist,fill="deeppink",colour="violet")
  })
  
  output$boxplot <- renderPlot({    
    if (is.null(input$archivo)) return(NULL)
    if (!input$todas_maquinas){
      rango_fechas = datos()$dia > input$periodo[1] & datos()$dia < input$periodo[2]
      datasubset <- datos()[rango_fechas & datos()$matricula==input$TipoSel,]
      ggplot(datasubset, aes(x=datasubset[,2],y=datasubset[,input$SelectAlarmaEst]))+
        geom_boxplot()+
        labs(x="factor(matricula)",y="eval(alarma)")
    }else{
      rango_fechas = datos()$dia > input$periodo[1] & datos()$dia < input$periodo[2]
      datasubset <- datos()[rango_fechas,]
      ggplot(datasubset, aes(x=datasubset[,2],y=datasubset[,input$SelectAlarmaEst]))+
        geom_boxplot()+
        labs(x="factor(matricula)",y="eval(alarma)")+
        theme(axis.text.x = element_text(angle=45, hjust=1)) 
    }
    
  })
  

  
}

shinyApp(ui = ui, server = server)

