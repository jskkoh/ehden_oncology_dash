# Cancer survival dashboard app
# Written by James Koh adapted from open source code by Paul Schneider

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(waiter)
library(tidyverse)
library(DT)
library(readxl)


# extrap_param = readRDS("./data/extrapolation_parameters_CPRD_Aurum.rds")

# Load data ---------------------------------------------------------------

# Goodness of fit statistics
gof_df_all = readRDS("./data/fitCprd.rds") 
# K-M survival curves
surv_df_all = readRDS("./data/survCprd.rds") 
# Hazard rate over time
rateHaz_df_all = readRDS("./data/rateHazCprd.rds")
# Mean/median survival estimates
survAvg_df_all = readRDS("./data/survAvgCprd.rds") 
# Number at risk summary table
riskTable_df_all = readRDS("./data/riskTableCprd.rds") 

# Load analysis functions
source("./R/cancerDashFunctions.R")
source("./R/plotCombinations.R")

# Load loading page and help box code
source("./R/textBoxes.R")
source("./R/introPage.R")

# Rename highchart download button
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)


# UI ----------------------------------------------------------------------

ui <- fillPage(
  
  # Use bootstrap 5
  suppressDependencies("bootstrap"),
  tags$script(src="www/utils.js"),
  tags$script(  src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js",
                integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM",
                crossorigin="anonymous"
  ),
  tags$title("EHDEN Cancer Survival Dashboard"),
  tags$link(
    href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
    rel="stylesheet",
    integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
    crossorigin="anonymous"
  ),
  # Enable shinyjs
  useShinyjs(),
  # Load javascript file
  includeScript("./www/cancerDash.js"),
  # Load CSS file
  includeCSS("style.css"),
  # Show loading screen
  use_waiter(),
  waiter_show_on_load(color = "", html = landingDiv()),
  
  div(
    class="main d-flex flex-row", 
    style="min-height: 100%;",
    
    # Input panel -------------------------------------------------------------
    div(
      class="d-flex flex-column border flex-grow-1 px-3",
      style="min-width: 250px; max-width: 300px; flex-basis: 100px; background-color:#360000",
      
      # Title
      HTML('
          <img  class ="image" src="ehden_logo.png">
          '), 
      div(
        class = "h3 mb-2 mt-3 text-left",
        "Cancer Survival Dashboard"
      ),
      
      # Action buttons
      div(
        class="d-flex  flex-row justify-content-center flex-wrap w-100 pt-3 border-top border-bottom",
        actionButton("info", "About",icon = icon("info-circle"), 
                     class = "btn-info-2 my-2", "data-bs-toggle"="modal", "data-bs-target"="#infoModal"),
        actionButton("sources", "Data sources", icon = icon("book"), 
                     class = "btn-info-2 my-2", "data-bs-toggle"="modal", "data-bs-target"="#sourcesModal"),
        
      ),
      
      # Inputs
      div(
        class = "d-flex flex-column justify-content-center input-bar",
        
        # Dataset
        div(
          class = "control-label text-left mb-2 mt-2 fw-bold",
          "Dataset"
        ),
        selectInput("dataset",NULL,selected="CPRD (UK)",
                    choices = list("CPRD (UK)" = "CPRD_Aurum")),
        
        # Cancer type
        div(
          class = "control-label text-left mb-2 mt-2 fw-bold",
          "Cancer type"
        ),
        selectizeInput(
          inputId = "cancerType", 
          label = NULL, 
          selected = "BreastCancer",
          choices = list(
            "Breast cancer" = "BreastCancer",
            "Colorectal cancer" = "ColorectalCancer",
            "Head and neck cancer" = "HeadNeckCancer",
            "Liver cancer" = "LiverCancer",
            "Lung cancer" = "LungCancer",
            "Pancreatic cancer" = "PancreaticCancer",
            "Prostate cancer" = "ProstateCancer",
            "Stomach cancer" = "StomachCancer"
          )
        ),
        
        
        # Age / sex
        div(
          class = "control-label text-left mb-2 mt-2 fw-bold",
          "Age group"
        ),
        selectInput("age",NULL,selected="All",
                    choices = list("All",
                                   "<30",
                                   "30-39",
                                   "40-49",
                                   "50-59",
                                   "60-69",
                                   "70-79",
                                   "80-89",
                                   ">=90")),
        div(
          class = "control-label text-left mb-2 mt-2 fw-bold",
          "Sex"
        ),
        selectInput("sex",NULL,selected="Both",
                    choices = list("Female",
                                   "Male",
                                   "Both")),
        
        div(
          class="d-flex  flex-row justify-content-center flex-wrap w-100 pt-3 mt-2 mb-2 border-top border-bottom",
          
          actionButton("ehden", "EHDEN network", icon = icon("paper-plane"), 
                       class = "btn-info-2 my-2")
        ),
      )
    ),
    
    
    # Results panel -----------------------------------------------------------
    
    div(
      class="d-flex flex-row flex-grow-1 flex-wrap align-items-start align-content-start  justify-content-center p-3",
      style="flex-basis: 300px; margin-top: 50px",
      
      
      # Survival curve extrapolation card
      div(
        class="res-card w-50",
        div(
          class = "res shadow border rounded-3 bg-white p-3 w-100",
          div(
            class = "fs-4 mb-3 mt-2 ms-2",
            textOutput("survPlotTitle")
          ),
          div(
            class = "control-label text-left mt-2 mb-2 mt-2 fw-bold",
            "Select time range"
          ),
          div(
            class = "control-label text-left mb-2 mt-2 fw-bold",
            sliderInput("survTimeRange",NULL,min=0, 
                        max=25,value=c(0, 25),step=1)
          ),
          highchartOutput("survivalPlot")
        )
      ),
      
      
      # Hazard plot card
      div(
        class="res-card w-50",
        div(
          class = "res shadow border rounded-3 bg-white p-3 w-100",
          div(
            class = "fs-4 mb-3 mt-2 ms-2",
            textOutput("hazPlotTitle")
          ),
          div(
            class = "control-label text-left mt-2 mb-2 mt-2 fw-bold",
            "Select time range"
          ),
          div(
            class = "control-label text-left mb-2 mt-2 fw-bold",
            sliderInput("hazTimeRange",NULL,min=0, 
                        max=25,value=c(0, 25),step=1)
          ),
          highchartOutput("hazardPlot")
        )
      ),
      
      # No. at risk table card
      div(
        class="res-card-risk w-50",
        div(
          class = "res shadow border rounded-3 bg-white p-3",
          div(
            class = "fs-4 mb-3 mt-2 ms-2",
            "Number at risk by time"
          ),
          
          div(
            class = "res-line justify-content-center",
            div(
              style="font-size:80%",
              DT::dataTableOutput("riskTable")
            )
          )
        )
      ),
      
      # Goodness of fit table card
      div(
        class="res-card-gof w-50",
        div(
          class = "res shadow border rounded-3 bg-white p-3",
          div(
            class = "fs-4 mb-3 mt-2 ms-2",
            "Extrapolation goodness-of-fit statistics"
          ),
          
          div(
            class = "res-line justify-content-center",
            div(
              style="font-size:80%",
              DT::dataTableOutput("fitTable")
            )
          )
        )
      )
      
    )
  ),
  
  
  
  modalDivs()
  
)

server = function(input, output, session){

  # Reactive data -----------------------------------------------------------
  reactiveData = reactiveValues()
  
  observe({
    
    # Use functions from 'cancerDashFunctions.R' to filter all data based on 
    # Age/Sex/Cancer/Dataset inputs
    reactiveData$tableSurvPlot = survPlotTable(
      surv_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$tableSurvPlotKM = survPlotTableKM(
      surv_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$tableHazPlot = hazPlotTable(
      rateHaz_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$tableHazObsPlot = hazPlotTableObs(
      rateHaz_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$tableGOF = gofTable(
      gof_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$tableRisk = riskTable(
      riskTable_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$tableSurvAvg = survAvgTable(
      survAvg_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    
    reactiveData$meanSurv =  pull(reactiveData$tableSurvAvg %>% 
                                    select(meanSurv))
    reactiveData$medianSurv =  pull(reactiveData$tableSurvAvg %>% 
                                      select(medianSurv))
    
    reactiveData$meanSurvText = if(is.na(reactiveData$medianSurv)) {
      paste0("Mean survival: <b>",reactiveData$meanSurv," years</b><br>")
    } else {
      paste0("Mean survival: <b>",reactiveData$meanSurv," years</b><br>",
             "Median survival: <b>",reactiveData$medianSurv," years</b>")
    } 
    
    reactiveData$subgroupPossible = pull(plotCombinations %>% 
                                           filter(Cancer==input$cancerType & 
                                                    Age==input$age & 
                                                    Gender==input$sex & 
                                                    Database==input$dataset) %>%
                                           select(Render)
    )
    
    
  })
  
  cancerName = reactive ({
    if(input$cancerType=="BreastCancer") {"breast cancer"}
    else if(input$cancerType=="ColorectalCancer") {"colorectal cancer"}
    else if(input$cancerType=="HeadNeckCancer") {"head and neck cancer"}
    else if(input$cancerType=="LiverCancer") {"liver cancer"}
    else if(input$cancerType=="LungCancer") {"lung cancer"}
    else if(input$cancerType=="PancreaticCancer") {"pancreatic cancer"}
    else if(input$cancerType=="ProstateCancer") {"prostate cancer"}
    else {"stomach cancer"}
  }) 
  
  
  # Text outputs ------------------------------------------------------------
  
  output$survPlotTitle = renderText({
    paste0("Observed and fitted survival for ",cancerName())
  })
  
  output$hazPlotTitle = renderText({
    paste0("Hazard function over time for ",cancerName())
  })
  
  
  
  
  # Tables ------------------------------------------------------------------
  
  output$riskTable = DT::renderDataTable({
    
    datatable(reactiveData$tableRisk,
              rownames = FALSE,
              options = list(paging=FALSE,searching=FALSE,info=FALSE))
  })
  
  output$fitTable = DT::renderDataTable({
    
    datatable(reactiveData$tableGOF,
              rownames = FALSE,
              options = list(paging=FALSE,searching=FALSE,info=FALSE))
  })
  
  
  
  
  # Highcharter plots -------------------------------------------------------
  
  output$survivalPlot = renderHighchart({
    highchartSurv()
  })
  
  output$hazardPlot = renderHighchart({
    highchartHaz()
  })
  
  # Survival curve extrapolation plot
  highchartSurv <- reactive({
    hchart(reactiveData$tableSurvPlot,"line",
           hcaes(
             x = time,
             y = est,
             group = Method
           ),
           color = c("#88CCEE","#CC6677","#DDCC77","#117733","#332288",
                     "#AA4499","#44AA99","#999933","#882255","#661100")
    ) %>%
      hc_add_series(
        reactiveData$tableSurvPlotKM,"line",
        hcaes(
          x = time,
          y = est
        ),
        name="Kaplan-Meier",
        id="km",
        lineWidth = 4,
        color = "#000000",
        zIndex = 1
      ) %>%
      hc_add_series(
        reactiveData$tableSurvPlotKM,"arearange",
        name="95% Confidence Interval",
        hcaes(
          x = time,
          low = lcl, high = ucl
        ),
        linkedTo = "km", 
        showInLegend = FALSE,
        color = hex_to_rgba("light gray", 0.02),  
        zIndex = 0
      ) %>%
      hc_title(text = reactiveData$meanSurvText,align="right",x=0,y=20,
               verticalAlign='top',floating="true", style=list(fontSize="14px") 
      ) %>%
      hc_tooltip(
        valueDecimals = 3
      ) %>%
      hc_boost(enabled = FALSE) %>%
      hc_yAxis(
        title = list(text = "Survival probability"),
        max=1,min=0
      ) %>%
      hc_xAxis(
        title = list(text = "Time (years)"),
        min=input$survTimeRange[1],max=input$survTimeRange[2]
      ) %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(
          target = "_blank"
        ),
        chartOptions = list(
          chart = list(
            backgroundColor = "white"
          )
        ),
        buttons = list(
          contextButton = list(
            symbol = "download",
            verticalAlign = "bottom",
            horizontalAlign = "left",
            onclick = JS("function () {
                     this.exportChart();
                 }")
          )
        )
      ) %>% 
      hc_add_theme(hc_theme(
        chart = list(
          style = list(
            fontFamily = "Arial"
          )
        )
      )
      )
  })
  
  
  # Hazard function plot
  highchartHaz <- reactive({
    hchart(reactiveData$tableHazPlot,"line",
           hcaes(
             x = time,
             y = est,
             group = Method
           ),
           color = c("#88CCEE","#CC6677","#DDCC77","#117733","#332288",
                     "#AA4499","#44AA99","#999933","#882255","#661100") 
    ) %>%
      hc_add_series(
        reactiveData$tableHazObsPlot,"line",
        name="Observed",
        id="obs",
        hcaes(
          x = time,
          y = est
        ),
        lineWidth = 4,
        color = "#000000",
        zIndex = 1
      ) %>%
      hc_add_series(
        reactiveData$tableHazObsPlot,"arearange",
        name="95% Confidence Interval",
        hcaes(
          x = time,
          low = lcl, high = ucl
        ),
        linkedTo = "obs", 
        showInLegend = FALSE,
        color = hex_to_rgba("light gray", 0.02),  
        zIndex = 0
      ) %>%
      hc_tooltip(
        valueDecimals = 3
      ) %>%
      hc_boost(enabled = FALSE) %>%
      hc_yAxis(
        title = list(text = "Hazard rate"),
        min=0
      ) %>%
      hc_xAxis(
        title = list(text = "Time (years)"),
        min=input$hazTimeRange[1],max=input$hazTimeRange[2]
      ) %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(
          target = "_blank"
        ),
        chartOptions = list(
          chart = list(
            backgroundColor = "white"
          )
        ),
        buttons = list(
          contextButton = list(
            symbol = "download",
            verticalAlign = "bottom",
            horizontalAlign = "left",
            onclick = JS("function () {
                     this.exportChart();
                 }")
          )
        )
      ) %>% 
      hc_add_theme(hc_theme(
        chart = list(
          style = list(
            fontFamily = "Arial"
          )
        )
      )
      )
    
  })
  
}

shinyApp(ui, server)