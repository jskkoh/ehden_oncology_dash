# Cancer survival dashboard app

library(tidyverse)
library(DT)
library(readxl)


# Define sample of time observations kept 
survFrac <- 0.01
# Load survival curve, hazard rate and GoF data 
survExCprd_df = read_csv("./data/survivalCurveExtrapolations.csv",col_types = cols()) %>%
  mutate(time=round(time,3),est=round(est,3),lcl=round(lcl,3),ucl=round(ucl,3)) %>%
  group_by(Method,Cancer,Age,Gender) %>%  sample_frac(survFrac, replace = FALSE) %>% 
  ungroup() %>% arrange(time)
rateHazCprd_df = read_csv("./data/survivalHazardRates.csv",col_types = cols()) %>%
  mutate(time=round(time,3),est=round(est,4),lcl=round(lcl,4),ucl=round(ucl,4)) %>%
  group_by(Method,Cancer,Age,Gender) %>%  sample_frac(survFrac, replace = FALSE) %>% 
  ungroup() %>% arrange(time)
fitCprd_df = read_csv("./data/survivalGOF.csv",col_types = cols()) %>%
  mutate(trisk=round(trisk,0),logLik=round(logLik,3),AIC=round(AIC,3),BIC=round(BIC,3))
kmCprd_df = read_csv("./data/survivalKM.csv",col_types = cols()) %>%
  mutate(time=round(time,3),est=round(est,3),lcl=round(lcl,3),ucl=round(ucl,3)) %>%
  group_by(Method,Cancer,Age,Gender) %>%  sample_frac(survFrac, replace = FALSE) %>% 
  ungroup() %>% arrange(time)
rateHazObsCprd_df = read_csv("./data/survivalHazardRatesObs.csv",col_types = cols()) %>%
  mutate(time=round(time,3),est=round(est,3),lcl=round(lcl,3),ucl=round(ucl,3)) %>%
  group_by(Method,Cancer,Age,Gender) %>%  sample_frac(survFrac, replace = FALSE) %>% 
  ungroup() %>% arrange(time)

# Load analysis functions
source("./R/cancerDashFunctions.R")

# Load loading page and help box code
source("./R/text_boxes.R")
source("./R/intro_page.R")


# Rename highchart download button
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)


shinyServer(function(input, output,session) {
  
  
  
  # Reactive data -----------------------------------------------------------
  
  reactiveData = reactiveValues()
  
  
  observe({
    
    if(input$dataset == "cprd"){
      survEx_df = survExCprd_df
      rateHaz_df = rateHazCprd_df
      fit_df = fitCprd_df
      km_df = kmCprd_df
      rateHazObs_df = rateHazObsCprd_df
    }
    
    reactiveData$tableSurvPlot = survPlotTable(
      survEx_df,
      input$cancerType,
      input$age,
      input$sex
    )
    
    reactiveData$tableSurvPlotKM = survPlotTableKM(
      km_df,
      input$cancerType,
      input$age,
      input$sex
    )
    
    reactiveData$tableHazPlot = hazPlotTable(
      rateHaz_df,
      input$cancerType,
      input$age,
      input$sex
    )
    
    reactiveData$tableHazObsPlot = hazPlotTable(
      rateHazObs_df,
      input$cancerType,
      input$age,
      input$sex
    )
    
    reactiveData$tableGOF = gofTable(
      fit_df,
      input$cancerType,
      input$age,
      input$sex 
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
  
  output$gofTableTitle = renderText({
    paste0("Survival curve extrapolation goodness-of-fit statistics")
  })
  
  
  
  # Tables ------------------------------------------------------------------
  
  output$fitTable = renderDataTable({
    
    withProgress(message = 'Loading data table',{
      table = reactiveData$tableGOF
      datatable(table,
                rownames = FALSE,
                options = list(paging=FALSE,searching=FALSE,info=FALSE))
    })
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
           color = c("#999999","#E69F00","#56B4E9","#009E73",
                     "#F0E442","#0072B2","#D55E00","#CC79A7")
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
        color = hex_to_rgba("light gray", 0.2),  
        zIndex = 0
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
        title = list(text = "Time"),
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
           color = c("#999999","#E69F00","#56B4E9","#009E73",
                     "#F0E442","#0072B2","#D55E00","#CC79A7")) %>%
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
        color = hex_to_rgba("light gray", 0.1),  
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