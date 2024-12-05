# Cancer survival dashboard app
# Written by James Koh adapted from open source code by Paul Schneider

library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(highcharter)
library(waiter)
library(tidyverse)
library(DT)
library(readxl)

# Load data ---------------------------------------------------------------

# Goodness of fit statistics
gof_df_all = read.csv("./data/fitAll.csv") 
# K-M survival curves
surv_df_all = read.csv("./data/survAll.csv") 
# Hazard rate over time
rateHaz_df_all = read.csv("./data/rateHazAll.csv")
# Mean/median survival estimates
survAvg_df_all = read.csv("./data/survAvgAll.csv") 
# Number at risk summary table
riskTable_df_all = read.csv("./data/riskTableAll.csv") 
# Available analyses summary table
analysesRun_df_all = read.csv("./data/analysesRunAll.csv")
# Demographics table
table1_df_all = read_xlsx("./data/table1All.xlsx")

# Load analysis functions
source("./R/cancerDashFunctions.R")

# Load theme colours and waiter text
source("./R/introPage.R")

# Rename highchart download button
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  
  
  # Dashboard header --------------------------------------------------------
  
  dashboardHeader(
    tags$li(class = "dropdown main-header logo"
    ),
    title = div(
      tags$img(src="ehden_logo.png",width="250px"),
      "Cancer survival dashboard",
    ),
    titleWidth = "600px"
  ),
  
  
  # Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    width = "300px",
    sidebarMenu(
      tags$li(class = "left-side, main-sidebar"
      ),
      id = "tabs",
      
      
      menuItem(
        tabName="moreInfo",
        "Info",
        startExpanded=F,
        div(
          actionButton("showAbout", "About the dashboard", icon = icon("info-circle"),
          )
        ),
        
        div(
          actionButton("ehden", "EHDEN network", icon = icon("paper-plane")
          )
        ),
        div(
          actionButton("showDatasets", "Data sources", icon = icon("book")
          )
        )
      ),
      
      menuItem(
        tabName="Data",
        "Data",
        startExpanded=T,
        selectInput("dataset","Dataset",selected="CPRD (UK)",
                    choices = list("CPRD (UK)" = "CPRD_GOLD",
                                   "CRN (Norway)" = "CRN",
                                   "GCR (Geneva)" = "GCR",
                                   "HUS (Helsinki)" = "HUS",
                                   "HUVM (Andalusia)" = "HUVM",
                                   "IMASIS (Barcelona)" = "IMASIS",
                                   "IPCI (Netherlands)" = "IPCI",
                                   "NCR (Netherlands)" = "NCR",
                                   "SIDIAP (Catalonia)" = "SIDIAP",
                                   "ULSEDV" = "ULSEDV",
                                   "ULSGE" = "ULSGE",
                                   "ULSM (Matosinhos)" = "ULSM",
                                   "ULSRA" = "ULSRA",
                                   "UTARTU" = "UTARTU")),
        div(class="mb-2",
            selectizeInput(
              "cancerType","Cancer",selected = "Breast",
              choices = list(
                "Breast" = "Breast",
                "Colorectal" = "Colorectal",
                "Head and neck" = "Head_and_neck",
                "Liver" = "Liver",
                "Lung" = "Lung",
                "Pancreatic" = "Pancreatic",
                "Prostate" = "Prostate",
                "Stomach" = "Stomach"
              )
            )
        )
      ),
      
      menuItem(
        tabName="strat",
        "Stratification",
        startExpanded=F,
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
        )
      ),
      
      menuItem(
        tabName="plotTime",
        "Time range",
        startExpanded=F,
        sliderInput("survTimeRange","Survival plot",min=0, 
                    max=ceiling(max(surv_df_all$time)),value=c(0, ceiling(max(surv_df_all$time))),step=1),
        sliderInput("hazTimeRange","Hazard plot",min=0, 
                    max=ceiling(max(surv_df_all$time)),value=c(0, ceiling(max(surv_df_all$time))),step=1)
      )
      
    ),
    
    HTML('
          <br>
          '), 
    div(
      class = " my-2",
      
      actionButton("run", 
                   span("Run Analysis",
                        style = "font-size: 20px; font-variant: small-caps"),
                   icon = icon("circle-right")
      )
      
    ),
    
    HTML('
          <br><br>
          ')
    
  ),
  
  # Dashboard body ----------------------------------------------------------
  
  dashboardBody(
    
    # Show loading screen
    use_waiter(),
    waiter_show_on_load(color = "", html = landingDiv()),
    
    useShinyjs(),
    
    
    # Load javascript file
    includeScript("./www/cancerDash.js"),
   
    
    # Load CSS file
    includeCSS("style.css"),
    
    box(
      title = textOutput("survPlotTitle"), 
      width=12, status="primary", collapsible=T,
      
      conditionalPanel(
        "input.run===0",
        id="click-prompt",
        span("Click 'Run analysis' to begin",
             style = "font-size: 20px; color: #00436C; font-weight: bold")
      )
      ,
      shinycssloaders::withSpinner(
        type = 2,
        color.background = "white",
        color = "#00436C",
        highchartOutput("survivalPlot",height="500px"),
      )
    ),
    box(
      title = textOutput("hazPlotTitle"), 
      width = 12, status ="primary", 
      collapsible=T,collapsed=T
      ,
      shinycssloaders::withSpinner(
        type = 2,
        color.background = "white",
        color = "#00436C",
        highchartOutput("hazardPlot",height="500px"),
      )
      ),
    
    fluidRow(
      column(width = 4,
             box(
               title="At risk", width=NULL, status ="primary", 
               collapsible=T, collapsed=T,
               div(
                 class = "res-line justify-content-center",
                 div(
                   style="font-size:100%; font-color:#00436C"
                   ,
                   DT::dataTableOutput("riskTable")
                 )
               )
             )
      ),
      
      column(width = 8,
             box(
               title="Goodness of fit", width=NULL, status="primary", 
               collapsible=T, collapsed=T,
               div(
                 class = "res-line justify-content-center",
                 div(
                   style="font-size:100%"
                   ,
                   DT::dataTableOutput("fitTable")
                 )
               )
             ),
             box(
               title = "Sample characteristics", 
               width=NULL, status="primary", collapsible=T,collapsed=T,
               div(
                 class = "res-line justify-content-center",
                 div(
                   style="font-size:100%"
                   ,
                   DT::dataTableOutput("descriptiveTable")
                 )
               )
             )
      ),
    )
  ),
  title="EHDEN Cancer Survival Dashboard"
  
)


# Server ------------------------------------------------------------------

server = function(input, output, session){
  # Reactive data -----------------------------------------------------------
  
  # Active info modals
  observeEvent(input$showAbout,{
    aboutUi()
  })
  
  observeEvent(input$showDatasets,{
    datasetsUi()
  })
  
  # Reset age/sex selections when other stratification is selected
  observeEvent(input$stratify, {
    reset("age")
    reset("sex")
  })
  
  # Reactive value to check whether stratification is available
  stratifyCheck <- reactive({
    pull(analysesRun_df_all %>%
           filter(Cancer==input$cancerType &
                    Age==input$age &
                    Sex==input$sex &
                    Database==input$dataset &
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
  
  tableDescriptiveStats <- eventReactive(input$run,{ 
    descriptiveStatsTable(
      table1_df_all,
      input$cancerType,
      input$dataset
    ) %>%
      select(-Cancer,-Database)
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
              options = list(paging=FALSE,searching=FALSE,info=FALSE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'color': '#00436C'});",
                               "}")))
  })
  
  output$fitTable = DT::renderDataTable({
    
    datatable(tableGOF(),
              rownames = FALSE,
              options = list(paging=FALSE,searching=FALSE,info=FALSE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'color': '#00436C'});",
                               "}")))
  })
  
  output$descriptiveTable = DT::renderDataTable({
    
    datatable(tableDescriptiveStats(),
              rownames = FALSE,
              options = list(paging=FALSE,searching=FALSE,info=FALSE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'color': '#00436C'});",
                               "}")))
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
  highchartSurv <- eventReactive(input$run,{
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
            fontFamily = "Inter"
          )
        )
      )
      )
  })
  
  
  # Hazard function plot
  highchartHaz <- eventReactive(input$run,{
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
            fontFamily = "Inter"
          )
        )
      )
      )
    
  })
}

shinyApp(ui, server)