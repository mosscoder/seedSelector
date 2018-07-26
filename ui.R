library(shiny)
library(leaflet)
library(htmlwidgets)

shinyUI(fluidPage(
  useShinyalert(),
  
  sidebarLayout(
    sidebarPanel(width=2,
                 fluidRow(
                   img(src="https://storage.googleapis.com/seedmapper_dat/usgs.log.png", height=81, width=180),
                   
                   uiOutput("loading"),
                   
                   conditionalPanel("!output.loading", 
                                    HTML('<b><font color="red">App may take a minute or more to initialize, please wait.</font></b>')),
                   
                   selectInput("boundSelect", label = ("How would you like to define your area of interest?"), 
                               choices = list("Lat/Long Slider Bars" = "slider", 
                                              "Spatial Polygon" = "poly"),
                               selected = "slider"),
                   
                   conditionalPanel( condition = "input.boundSelect == 'poly'",             
                                     fileInput("boundFile2", 
                                               label = "Upload spatial polygon files (.shp, .shx, .prj, and .dbf) compressed into a .zip format:")),
                   
                   conditionalPanel(condition = "input.boundSelect == 'slider'",
                                    sliderInput("lat.range",
                                                label = "Latitude Extent", 
                                                min = 15, max = 60, step = 0.1,
                                                value=c(32,42))),
                   
                   conditionalPanel(condition = "input.boundSelect == 'slider'",  
                                    sliderInput("lon.range",
                                                label = "Longitude Extent", 
                                                min =-135, max = -45, step = 0.1,
                                                value=c(-115,-105))),
                   
                   img(src="https://storage.googleapis.com/seedmapper_dat/df.example.png", height = 48, width = 198),
                   
                   fileInput("accession.csv", 
                             label ="Now upload a .csv of your accession data (maximum of 50 accessions) in the format pictured above (id, long, lat):",
                             accept = ".csv"),
                   
                   actionButton("goButton", label=HTML("<b>Match Seed to Climate</b>"), 
                                style = "background-color: #18B66A; color: #fff; border-color: #ffffff; width: 190px"),
                   
                   
                   downloadButton('downloadData', 'Download Data', style = ' width: 190px;'),
                 
                 helpText("Click above to download underlying rasters and summary data. Note that clicking will open a new tab."),
                 
                 actionButton('moreInfo', label = HTML("<b>Contact Info & Disclaimer</b>"),
                              style = 'background-color: #008CBA; color: #fff;border-color: #ffffff; width: 190px'))
                 
                 #tags$hr(),
                 
                # tags$b("Optional: You may rank the climate match of your accessions at a specified set of coordinates:"),
                 #helpText(""),
                  # numericInput("rankLong", "Longitude", value=NULL, width="90px"),
                        
                  # numericInput("rankLat", "Latitude", value=NULL, width="90px")
                ),
    
    mainPanel(width = 10,
      
    tabsetPanel(id = "tabs1",
    tabPanel("Map", id="map", leafletOutput("leafmap",width="100%", height = "700px") %>% withSpinner(size = 3)),
    tabPanel("Accession Climate Data", id="table", dataTableOutput("centerTable")),
    tabPanel("Accession Assignment Distributions", id="box", plotOutput("boxPlot", height=2000) %>% withSpinner( size = 20)),
    tabPanel("Background and Use", id="background", includeText("instruct.txt"))
          )
    
    )
    
  ),
  
  uiOutput("condPanel")
  #absolutePanel(top="405px", left="270px",
   #             conditionalPanel(condition = "input.tabs1 == 'Map' && !is.null(output.leafmap)",
    #                             radioButtons(inputId="clickButton",
     #                                         label="Click Generates:",
      #                                        choices = list(
       #                                         "Rankings" = "rank",
        #                                        "Climate Data" = "clim"
         #                                     )))),
  

))