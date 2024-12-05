# Functions for EDHDEN cancer survival dashboard

# Table for survival plot
survPlotTable <- function(dataSurv,cancerType,age,sex,dataset) {
  tableSurvPlot <- dataSurv %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method!="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableSurvPlot)
}

survPlotTableKM <- function(dataKM,cancerType,age,sex,dataset) {
  tableKMPlot <- dataKM %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method=="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableKMPlot)
}

hazPlotTable <- function(dataHaz,cancerType,age,sex,dataset) {
  tableHazPlot <- dataHaz %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method!="Observed") %>%
    select(time,est,lcl,ucl,Method) %>% arrange(Method,time)
  return(tableHazPlot)
}

hazPlotTableObs <- function(dataHazObs,cancerType,age,sex,dataset) {
  tableHazObsPlot <- dataHazObs %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method=="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableHazObsPlot)
}

gofTable <- function(dataGOF,cancerType,age,sex,dataset) {
  tableGOF <- dataGOF %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & Database==dataset) %>%
    select(Method,logLik,AIC,BIC)
  return(tableGOF)
}

riskTable <- function(dataRisk,cancerType,age,sex,dataset) {
  tableRisk <- dataRisk %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & Database==dataset) %>%
    select(time,n.risk,n.event) %>% 
    mutate(time=as.numeric(time)) %>%
    arrange(time) %>%
    rename("Time (years)"=time,"No. at risk"=n.risk,"No. of events"=n.event)
  
  return(tableRisk)
}


survAvgTable <- function(dataSurvAvg,cancerType,age,sex,dataset) {
  tableRisk <- dataSurvAvg %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & Database==dataset) %>%
    return(tableRisk)
}

descriptiveStatsTable <- function(dataDescriptiveStats,cancerType,dataset) {
  tableDescriptive <- dataDescriptiveStats %>% 
    filter(Cancer==cancerType & Database==dataset) %>%
    return(tableDescriptive)
}


# UI functions ------------------------------------------------------------


aboutUi <- function() {
  showModal(modalDialog(
    size = "l",
    fade = T,
    
    title = div(style = "color: black; font-weight: bold",
                "About"),
    div(
      style = "color: black",
      div(
        h4(style = "color: black",
           "About this application"),
        HTML(
          "<p>
          This dashboard was developed by James Koh, Ravinder Claire, 
          Jeremy Dietz from the National Institute of Health and Care 
          Excellence and Danielle Newby from the University of Oxford.
          </p>
          "
        ),
        br(),
      ),
      div(
        h4(style = "color: black",
           "Acknowledgements"),
        HTML(
          "<p>We acknowledge the contributors from each of the EHDEN data partners 
          who produced the data that is provided within the dashboard. More information
          on the data partners and the datasets can be found in the 'Data sources' section</p>"
        ),
        br(),
      ),
      
      
      
      div(
        h4(style = "color: black",
           "Funding statement"),
        HTML(
          "<p>Financial support for this project was provided by the 
          European Health Data Evidence Network. All errors and opinions 
          represented in the application are entirely those of development team 
          and do not reflect those of European Health Data Evidence Network.</p>"
        ),
        br(),
      ),
      br()
      
      
    ),
    easyClose = TRUE,
    footer = div(modalButton("Close"), class = "border rounded-3")
  ))
}


datasetsUi <- function() {
  showModal(
    modalDialog(
      size = "l",
      fade = T,
      title = div(
        style = "color: black; font-weight: bold",
        "Datasets used in this dashboard"
      ),
      div(style = "color: black",
          tags$div(
            HTML(
              "<p>There are 14 datasets from across Europe that are
            included in this dashboard.</p>"
            )
          ),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.cprd.com/", "The Clinical Practice Research Datalink (CPRD) GOLD", target =
                      "_blank")),
          h5(style = "color: black","United Kingdom"),
          
          HTML(
            "<p>A primary care database of pseudo-anonymised patient-level information on demographics, 
            lifestyle data, clinical diagnoses, prescriptions, and preventive care from general practitioners (GPs)"
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.ipci.nl/", "The Integrated Primary Care Information (IPCI) database", target =
                      "_blank")),
          h5(style = "color: black","Netherlands"),
          HTML(
            "<p> Electronic healthcare records of patients registered with GPs throughout the Netherlands. The database 
            contains 2.8 million patients records and 1.1 million active patients (6.1% of the Dutch population)."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "http://www.sidiap.org/", "The Information System for Research in Primary Care (SIDIAP)", target =
                      "_blank")),
          h5(style = "color: black","Catalonia, Spain"),
          HTML(
            "<p>A database of population-wide primary care electronic health records. The database contains pseudo-anonymized 
            records for >8 million people since 2006, with 5.8 million people active in June 2021"
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.unige.ch/medecine/rgt/accueil", "The Geneva Cancer Registry (GCR)", target =
                      "_blank")),
          h5(style = "color: black","Geneva, Switzerland"),
          HTML(
            "<p>A database covering all invasive, in situ and some benign cancer cases in the canton of Geneva. It contains 
            records for 150,000 patients, including information on sociodemographics, tumor characteristics, treatment setting and treatments."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.hospitalmacarena.es/", "Virgen Macarena University Hospital (HUVM)", target =
                      "_blank")),
          h5(style = "color: black","Seville, Spain"),
          HTML(
            "<p>A hospital EHR system containing more than 10 million episodes, 1 million discharge summaries and 1.1 million 
            consultations. The database includes information on complex diagnosis and treatments applied to cancer patients."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.ulsm.min-saude.pt/", "Unidade Local de Saúde de Matosinhos (ULSM)", target =
                      "_blank")),
          h5(style = "color: black","Matosinhos, Portugal"),
          HTML(
            "<p>The database of a large health care organization including 14 primary care centres assisted by one hospital that provides 
            primary, secondary, and tertiary care services."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.hus.fi/en", "Helsinki University Hospital (HUS)", target =
                      "_blank")),
          h5(style = "color: black","Helsinki, Finland"),
          HTML(
            "<p>An EHR database of the biggest health care provider in Finland, rresponsible for organizing specialized 
            healthcare in the Uusimaa region. It contains information on visits, procedures and treatments of all patients who visit
            HUS hospitals."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://iknl.nl/en/ncr", "The Netherlands Cancer Registry (NCR)", target =
                      "_blank")),
          h5(style = "color: black","Netherlands"),
          HTML(
            "<p>The NCR is the only oncological hospital registry in the Netherlands with data on all cancer patients aged 18 years
            and over at time of diagnosis from 1989 onwards."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.kreftregisteret.no/en/", "The Cancer Registry of Norway (CRN)", target =
                      "_blank")),
          h5(style = "color: black","Norway"),
          HTML(
            "<p>A national database containing multiple linked sources of electronic health data including pathology laboratories, 
            clinical notifications, radiotherapy machines and Norwegian population and cause of death registries."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "https://www.parcdesalutmar.cat/es/hospitals/hospital-del-mar/", "Institut Municipal d'Assistència Sanitària (IMASIS)", target =
                      "_blank")),
          h5(style = "color: black","Barcelona, Spain"),
          HTML(
            "<p>The IMASIS information system contains EHRs for the Parc Salut Mar Barcelona, a complete healthcare services organisation. 
            It covers >1 million patients from 1990 onwards."
          ),
          HTML("</p>"),
          br(),
          
          h4(style = "color: black",
             tags$a(href = "          https://www.kliinikum.ee/vahikeskus/teadus-ja-innovatsioon/", "Tartu University Hospital cancer center (UTARTU)", target =
                      "_blank")),
          h5(style = "color: black","Estonia"),
          HTML(
            "<p>A dataset containing all the cases of prostate, lung and breast cancer in Estonia between 2012 and 2022. 
            The EHRs comprise information from primary and secondary care, sourced from insurance claims, discharge summaries, 
            prescriptions and the national deaths registry."
          ),
          HTML("</p>"),

          br(),
          br()
      ),
      easyClose = TRUE,
      footer = div(modalButton("Close"), class = "border rounded-3")
    )
  )
}
