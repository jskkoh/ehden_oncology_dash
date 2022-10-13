
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(waiter)

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
      div(
        class = "h3 mb-2 mt-3 text-left fw-bold",
        "EHDEN Cancer Survival Dashboard"
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
                    choices = list("CPRD (UK)" = "cprd")),
        
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
                    choices = list("50-69",
                                   "70+",
                                   "All")),
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
  
  
  
  # RESULTS PANEL ------    
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
        sliderInput("survTimeRange",NULL,min=0, 
                    max=25,value=c(0, 25),step=1),
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
        sliderInput("hazTimeRange",NULL,min=0, 
                    max=25,value=c(0, 25),step=1),
        highchartOutput("hazardPlot")
      )
    ),
    
    
    
   
    
    
    

    
    
  )
),



modalDivs()

)