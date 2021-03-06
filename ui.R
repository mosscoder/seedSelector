ui <- fluidPage(
  useShinyalert(),
  sidebarLayout(
    
    sidebarPanel(width=2,
                 
                 fluidRow(
                   img(src="https://github.com/mosscoder/seedselector/blob/ssDev/cpnpp_logo.png?raw=true", height=81, width='100%'),
                   img(src="https://github.com/mosscoder/seedselector/blob/master/usgs.log.png?raw=true", height=81, width='100%'),
                   
                   tags$div(id = 'selectDiv', selectInput("boundSelect", 
                                             label = ("How would you like to define your area of interest?"), 
                               choices = list("Lat/Long Slider Bars" = "slider", 
                                              "Spatial Polygon" = "poly"),
                               selected = "slider")),
                   tooltip(refId = 'selectDiv', "Analyses limited to land masses of North America."),
                   
                   conditionalPanel( condition = "input.boundSelect == 'poly'",             
                                     tags$div(id = 'polyDiv', fileInput("boundFile2", 
                                               label = "Upload spatial polygon:",
                                               accept = ".zip"))),
                   tooltip(refId = 'polyDiv', 'Compress the following components into a .zip file:<br><br>.shp<br>.shx<br>.prj<br>.dbf'),
                   
                   conditionalPanel(condition = "input.boundSelect == 'slider'",
                                    sliderInput("lat.range",
                                                label = "Latitude Extent", 
                                                min = 7, max = 83, step = 0.1,
                                                value=c(32,40))),
                   
                   conditionalPanel(condition = "input.boundSelect == 'slider'",  
                                    sliderInput("lon.range",
                                                label = "Longitude Extent", 
                                                min =-168, max = -52, step = 0.1,
                                                value=c(-115,-105))),
                   
                   tags$div(id ='upload1', fileInput("accession", 
                             label ="Upload accession origins:",
                             accept = ".csv")),
                   
                   tooltip(refId = 'upload1', html = TRUE, delay = 50, placement = 'top',
                           text = HTML('A maximum of 50 accessions is supported. Upload a spreadsheet of .csv file format containing three columns:<br><br>id (name of the accession)<br>x (decimal degrees longitude)<br>y (decimal degrees latitude)<br><br><img src="https://storage.googleapis.com/seedmapper_dat/df.example.png" height = 48 width = "100%"/>')),
                   
                   HTML(paste0('<button data-toggle="collapse" class="btn btn-default" id="wtButton" 
                               data-target="#demo"><img src="https://github.com/mosscoder/seedselector/blob/master/scale.png?raw=true" height = 20, width 20/><span>Weight Variables</span></button>')),
                   tags$style(type = "text/css", "#wtButton {width: 100%; display: inline-block; padding: 5px; margin: 2px;}"),
                   
                   tags$div(id = 'demo',  class="collapse out",
                            sliderInput("wtMAT", label = "Mean Annual Temperature", 
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtDiurnal", label = "Diurnal Range", 
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtTSeason", label = "Temperature Seasonality", 
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtTWet", label = "Temperature Wettest Qtr.",
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtMAP", label = "Mean Annual Precipitation",
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtPSeason", label = "Precipitation Seasonality",
                                        ticks = F, value=1, min = 0, max = 1, step = .01),
                            sliderInput("wtPWarm", label = "Precipitation Warmest Qtr.",
                                        ticks = F, value=1, min = 0, max = 1, step = .01)        
                   ),
                   
                   tooltip(refId ='wtButton', 'Change the importance of any variable during similarity calculations. Weights set to 0.5 have half the importance relative to 1, 0.25 a quarter, and 0.1 a tenth. '),
                   
                   actionButton("goButton", label=HTML("<b>Match Seed to Climate</b>")),
                   
                   downloadButton('downloadData', 'Download Data', style = ' width: 100%; margin: 2px;'),
                   
                   tooltip(refId ='downloadData',
                           "Click to download analysis products (opens a new tab). Includes: raster overlays, accession climate data, and within-assignment distribution boxplots."),
                   hr(),
                   actionButton('moreInfo', label = HTML("<font size = 2><b>Contact Info & Disclaimer</b></font>")),
                   
                   tags$style(type = "text/css", '#goButton{background-color: #18B66A; color: #fff; border-color: #ffffff; width: 100%;margin: 2px;}
                              #goButton:hover{background-color: #1EE285;}
                              #moreInfo{background-color: #008CBA; color: #fff;border-color: #ffffff; width: 100%;margin: 2px; padding:5px}
                              #moreInfo:hover{background-color: #00B1ED;}')
                   )), 
    
    mainPanel(width = 10,
              
              tabsetPanel(id = "tabs",
                          tabPanel("Map", id="map", leafletOutput("leaf",width="100%", height = "700px") %>% withSpinner(size = 3)),
                          tabPanel("Accession Climate Data", dataTableOutput("accessionTable")),
                          tabPanel("Within-Assignment Distributions", id="box", 
                                   fluidRow(
                                     column(2,selectInput("ggVar1", label = ("Variable 1"), 
                                                          choices = list("MAT" = "MAT",
                                                                         "DiurnalRange" = "DiurnalRange",
                                                                         "TSeasonality" = "TSeasonality",
                                                                         "TWettestQtr" = "TWettestQtr",
                                                                         "MAP" = "MAP",
                                                                         "PSeasonality" = "PSeasonality",
                                                                         "PWarmestQtr" = "PWarmestQtr"),
                                                          selected = "MAT")),
                                     column(2,selectInput("ggVar2", label = ("Variable 2"), 
                                                          choices = list("MAT" = "MAT",
                                                                         "DiurnalRange" = "DiurnalRange",
                                                                         "TSeasonality" = "TSeasonality",
                                                                         "TWettestQtr" = "TWettestQtr",
                                                                         "MAP" = "MAP",
                                                                         "PSeasonality" = "PSeasonality",
                                                                         "PWarmestQtr" = "PWarmestQtr"),
                                                          selected = "MAP")),
                                     column(2,selectInput("ggVar3", label = ("Variable 3"), 
                                                          choices = list("MAT" = "MAT",
                                                                         "DiurnalRange" = "DiurnalRange",
                                                                         "TSeasonality" = "TSeasonality",
                                                                         "TWettestQtr" = "TWettestQtr",
                                                                         "MAP" = "MAP",
                                                                         "PSeasonality" = "PSeasonality",
                                                                         "PWarmestQtr" = "PWarmestQtr"),
                                                          selected = "PSeasonality"))),
                                   plotOutput("boxPlot", height=650) %>% withSpinner( size = 3)),
                          tabPanel("Background and Use", id="background", 
                                   HTML('<p align="center"><iframe width="750px" height="500px" src="https://www.youtube.com/embed/1QYI-X6imp0" frameborder="10" allow="autoplay; encrypted-media" allowfullscreen></iframe></p>'),
                                   textOutput('instruct')),
                          tabPanel("Run Offline", id="offline",
                                   tags$iframe(src='https://rawgit.com/mosscoder/seedselector/master/offlineInstructions.html',
                                               width = "100%", height = "1000px", style="border:0"))
              ))
                   )
  )