# Cancer survival dashboard app
# Written by James Koh adapted from open source code by Paul Schneider

library(shiny)
library(bslib)
library(bsicons)
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
gof_df_all = read.csv("./data/fitCprd.csv") 
# K-M survival curves
surv_df_all = read.csv("./data/survCprd.csv") 
# Hazard rate over time
rateHaz_df_all = read.csv("./data/rateHazCprd.csv")
# Mean/median survival estimates
survAvg_df_all = read.csv("./data/survAvgCprd.csv") 
# Number at risk summary table
riskTable_df_all = read.csv("./data/riskTableCprd.csv") 

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
      style="min-width: 250px; max-width: 300px; flex-basis: 100px; background-color:#00436C",
      
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
                    choices = list("CPRD (UK)" = "CPRD_GOLD")),
        
        # Cancer type
        div(
          class = "control-label text-left mb-2 mt-2 fw-bold",
          "Cancer type"
        ),
        selectizeInput(
          inputId = "cancerType", 
          label = NULL, 
          selected = "Breast",
          choices = list(
            "Breast cancer" = "Breast",
            "Colorectal cancer" = "Colorectal",
            "Head and neck cancer" = "Head_and_neck",
            "Liver cancer" = "Liver",
            "Lung cancer" = "Lung",
            "Pancreatic cancer" = "Pancreatic",
            "Prostate cancer" = "Prostate",
            "Stomach cancer" = "Stomach"
          )
        ),
        
        # Age / sex
        div(
          class = "control-label text-left mb-2 mt-2 fw-bold",
          "Stratification"
          ),
        selectInput("stratify",NULL,selected="None",
                    choices = list("None",
                                   "Age",
                                   "Sex")),
        
        conditionalPanel(
          condition = "input.stratify=='Age'",
          div(
            class = "control-label text-left mb-2 mt-2 fw-bold",
            "Age group"
          ),
          prettyRadioButtons(
            inputId="age",label=NULL,selected="All",
            choices = list("All",
                           "18 to 39",
                           "40 to 49",
                           "50 to 59",
                           "60 to 69",
                           "70 to 79",
                           "80 +"),
            status = "primary",
            icon = icon("check")
          )
        ),
        conditionalPanel(
          condition = "input.stratify=='Sex'",
          div(
            class = "control-label text-left mb-2 mt-2 fw-bold",
            "Sex"
          ),
          prettyRadioButtons(
            inputId="sex",label=NULL,selected="Both",
            choices = list("Both",
                           "Male",
                           "Female"),
            status = "primary",
            icon = icon("check")
          )
        ),
        
        HTML('
          <br>
          '), 
        actionButton("run", "Run analysis",icon = icon("circle-right"), 
                     class = "btn-run my-2"),
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
      style="flex-basis: 300px; margin-top: 50px; background-color:#228096",
      
      
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
                        max=ceiling(max(surv_df_all$time)),value=c(0, ceiling(max(surv_df_all$time))),step=1)
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
                        max=ceiling(max(surv_df_all$time)),value=c(0, ceiling(max(surv_df_all$time))),step=1)
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
    
  # Reset age/sex selections when other stratification is selected
  observeEvent(input$stratify, {
    reset("age")
    reset("sex")
  })
  
  # Reactive value to check whether stratification is available
  stratifyCheck <- reactive({
    pull(analysesRunList %>%
           filter(Cancer==input$cancerType &
                    Age==input$age &
                    Sex==input$sex &
                    Method=="Kaplan-Meier") %>%
           select(Run)
           )
  })
 
  # Enable/disable action button based on stratification availability
  observe({
    toggleState(id="run", condition=stratifyCheck()=="Yes")
  })
 
    # Use functions from 'cancerDashFunctions.R' to filter all data based on 
    # Age/Sex/Cancer/Dataset inputs
    tableSurvPlot <- eventReactive(input$run,{ 
      survPlotTable(
      surv_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    tableSurvPlotKM <- eventReactive(input$run,{ 
    survPlotTableKM(
      surv_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    tableHazPlot <- eventReactive(input$run,{ 
      hazPlotTable(
      rateHaz_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    tableHazObsPlot <- eventReactive(input$run,{ 
      hazPlotTableObs(
      rateHaz_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    tableGOF <- eventReactive(input$run,{ 
      gofTable(
      gof_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    tableRisk <- eventReactive(input$run,{ 
      riskTable(
      riskTable_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    tableSurvAvg <- eventReactive(input$run,{ 
      survAvgTable(
      survAvg_df_all,
      input$cancerType,
      input$age,
      input$sex,
      input$dataset
    )
    })
    
    meanSurv<- eventReactive(input$run,{ 
      pull(tableSurvAvg() %>% 
             select(meanSurv))
    })
    medianSurv <- eventReactive(input$run,{ 
      pull(tableSurvAvg() %>% 
             select(medianSurv))
    })
    
    meanSurvText <- eventReactive(input$run,{ 
      if(is.na(medianSurv())) {
      paste0("Mean survival: <b>",meanSurv()," years</b><br>")
    } else {
      paste0("Mean survival: <b>",meanSurv()," years</b><br>",
             "Median survival: <b>",medianSurv()," years</b>")
    } 
    })
  
  cancerName <- eventReactive(input$run, { 
    if(input$cancerType=="Breast") {"breast cancer"}
    else if(input$cancerType=="Colorectal") {"colorectal cancer"}
    else if(input$cancerType=="Head_and_neck") {"head and neck cancer"}
    else if(input$cancerType=="Liver") {"liver cancer"}
    else if(input$cancerType=="Lung") {"lung cancer"}
    else if(input$cancerType=="Pancreatic") {"pancreatic cancer"}
    else if(input$cancerType=="Prostate") {"prostate cancer"}
    else {"stomach cancer"}
  }) 
  
  
  # Text outputs ------------------------------------------------------------
  
  output$survPlotTitle = renderText({
    paste0("Observed and fitted survival for ",cancerName())
  })
  
  output$hazPlotTitle = renderText({
    paste0("Hazard function over time for ",cancerName())
  })
  
  output$stratifyChecker = renderPrint({ 
    stratifyCheck()
  })
  
  
  # Tables ------------------------------------------------------------------
  
  output$riskTable = DT::renderDataTable({
    
    datatable(tableRisk(),
              rownames = FALSE,
              options = list(paging=FALSE,searching=FALSE,info=FALSE))
  })
  
  output$fitTable = DT::renderDataTable({
    
    datatable(tableGOF(),
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
  
  colorSet = c("#88CCEE","#CC6677","#DDCC77","#117733","#332288","#AA4499",
               "#44AA99","#999933","#882255","#661100")
  # Survival curve extrapolation plot
  highchartSurv <- reactive({
    hchart(tableSurvPlot(),"line",
           hcaes(
             x = time,
             y = est,
             group = Method
           ),
           color = colorSet[1:length(unique(tableSurvPlot()$Method))]
    ) %>%
      hc_add_series(
        tableSurvPlotKM(),"line",
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
        tableSurvPlotKM(),"arearange",
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
      hc_title(text = meanSurvText(),align="right",x=0,y=20,
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
    hchart(tableHazPlot(),"line",
           hcaes(
             x = time,
             y = est,
             group = Method
           ),
           color = colorSet[1:length(unique(tableHazPlot()$Method))]
           
    ) %>%
      hc_add_series(
        tableHazObsPlot(),"line",
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
        tableHazObsPlot(),"arearange",
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