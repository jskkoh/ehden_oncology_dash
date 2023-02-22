# Cancer survival dashboard app

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

# Load loading page and help box code
source("./R/textBoxes.R")
source("./R/introPage.R")

# Rename highchart download button
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)


shinyServer(function(input, output,session) {
  
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
  
})