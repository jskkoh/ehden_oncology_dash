library(dashboardthemes)

customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Inter"
  ,appFontColor = "#FFFFFF"
  ,primaryFontColor = "#00436C"
  ,infoFontColor = "#00436C"
  ,successFontColor = "#FFFFFF"
  ,warningFontColor = "#00436C"
  ,dangerFontColor = "#00436C"
  ,bodyBackColor = "#00436C"
  
  ### header
  ,logoBackColor = "#FFFFFF"
  ,headerButtonBackColor = "#FFFFFF"
  ,headerButtonIconColor = "#00436C"
  ,headerButtonBackColorHover = "#00436C"
  ,headerButtonIconColorHover = "#FFFFFF"
  
  ,headerBackColor = "#FFFFFF"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#00436C"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "#00436C"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "#FFFFFF"
  ,sidebarTabTextSize = 18
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "#0064A1"
  ,sidebarTabTextColorSelected = "#FFFFFF"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "#008BE0"
    ,colorMiddle = "#0064A1"
    ,colorEnd = "#00436C"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "#FFFFFF"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "#00436C"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 20
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "#008BE0"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "#00436C"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "#FFFFFF"
  ,buttonTextColor = "#00436C"
  ,buttonBorderColor = "#000000"
  ,buttonBorderRadius = 10
  
  ,buttonBackColorHover = "#0064A1"
  ,buttonTextColorHover = "#FFFFFF"
  ,buttonBorderColorHover = "#000000"
  
  ,textboxBackColor = "#008BE0"
  ,textboxBorderColor = "#FFFFFF"
  ,textboxBorderRadius = 10
  ,textboxBackColorSelect = "#008BE0"
  ,textboxBorderColorSelect = "#FFFFFF"
  
  ### tables
  ,tableBackColor = "#008BE0"
  ,tableBorderColor = "#00436C"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


