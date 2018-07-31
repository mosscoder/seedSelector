server <- shinyServer(function(input, output, session) {
  shinyalert(title = 'Welcome to Seed Selector!',
             text = HTML('App is initialized!<br><br>
                         For a detailed explanation of the underlying analyses, see
                         <a href="https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/eap.1505">Doherty et al. (2017)</a><br><br>
                         If you want to process a very large extent, it is best to run the app offline.<br>
                         <a href="https://rawgit.com/mosscoder/seedselector/master/offlineInstructions.html">
                         Click here for more information!</a>'
             ),
             type = 'success',
             closeOnClickOutside = TRUE,
             html = T)
  
  observeEvent(input$moreInfo, {
    showModal(modalDialog(HTML('This tool is intended for use 
                               by USGS personnel, academia, the native seed industry, and the 
                               public. The analyses presented here are  
                               based upon the methods described in 
                               <a href="https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/eap.1505">Doherty et al. (2017)</a>.
                               Please send questions, 
                               comments, suggestions for improvements, and error reports via 
                               email to USGS - Southwest Biological Science Center c/o Kyle 
                               Doherty (<a href="mailto:kyledaviddoherty@gmail.com">kyledaviddoherty@gmail.com</a>). 
                               The current web location for this tool is temporary and it will be 
                               hosted on a USGS server as soon as a suitable one can be located.<br><br>
                               Written by Kyle Doherty, U.S. Geological 
                               Survey, Southwest Biological Science Center, Flagstaff, Arizona. Written in the programming 
                               language R (R Core Team (2015). R: A language and environment for 
                               statistical computing. R Foundation for Statistical Computing,
                               Vienna, Austria. URL http://www.R-project.org/).<br><br>
                               
                               Disclaimer: Although this program has been used by the USGS, no 
                               warranty, expressed or implied, is made by the USGS or the United 
                               States Government as to the accuracy and functioning of the 
                               program and related program material nor shall the fact of 
                               distribution constitute any such warranty, and no responsibility is 
                               assumed by the USGS in connection therewith.
                               '
    ),
    easyClose = TRUE,
    footer = HTML("<button type='button' class='btn btn-success' data-dismiss='modal'>OK</button>")
    ))
  })
  
  output$instruct <- renderText('The purpose of this app is to aid seed selection efforts of restoration and 
                                revegetation practitioners by matching the climate of georeferenced seed sources 
                                to potential target sites. Analyses are conducted on the Bioclim 
                                (http://www.worldclim.org/bioclim) dataset for the extent of North America 
                                (-168 to -52 degrees longitude and 7 to 83 degrees latitude).  
                                We chose to incorporate seven of these
                                variables that together capture unique axes of multivariate climate space, 
                                including: mean annual temperature, diurnal range, temperature seasonality, 
                                temperature of warmest quarter, mean annual precipitation, precipitation
                                seasonality, and precipitation of warmest quarter. Climate match is assessed 
                                based upon a climate similarity index derived from these Bioclim variables. 
                                To operate the app, the user may input a lat/long bounding box with the supplied
                                slider bars or a spatial polygon for their extent of interest. They then upload a 
                                spreadsheet in .csv file format containing three columns: accession id, decimal degrees 
                                longitude, and decimal degrees latitude. The user then clicks the “Match Seed to Climate”
                                button, and the app maps the closest climate match of supplied accessions across the area of
                                interest. By clicking a point on the map, a popup appears with coordinates and climate match 
                                rankings for all uploaded accessions. Also reported are the corresponding Bioclim data for 
                                each of the accessions, as well as the distributions of their assignments. The user can then 
                                download and explore the underlying rasters, climate data of accessions, and assignment 
                                distributions for offline use. To reproduce the aesthetics of the mapping feature, set high 
                                values of the simval.tif as black and low values as white, then overlay the
                                accession.assignment.tif, setting it to ~50% transparency, with values as categorical, 
                                each value a contrasting color. A file is supplied to relate accession id to its 
                                corresponding value in the the accession.assignment.tif.')
  
  output$inc<-renderUI({includeHTML("https://rawgit.com/mosscoder/climpart/master/offlineInstructions.html")})
  
  output$leaf <- renderLeaflet({
    leaflet() %>%
      setView(lat = 50, lng = -100, zoom = 3) %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
      addProviderTiles("CartoDB.Positron", group = "Light Basemap") 
  })
  
  observe({
    if(input$boundSelect == "slider"){ 
      leafletProxy('leaf') %>%
        clearShapes() %>%
        addRectangles(lng1 = input$lon.range[1],
                      lng2 = input$lon.range[2],
                      lat1 = input$lat.range[1],
                      lat2 = input$lat.range[2],
                      color = '#317873',
                      weight = 2,
                      options =  pathOptions(pane = "tilePane", zIndex = 1000))
    } else if 
    
    (!is.null(input$boundFile2) & input$boundSelect == "poly"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing user polygon", value = 0.5)
      
      garbageList <- dir(path = temp.folder)
      
      file.remove(paste0(temp.folder,'/',garbageList))
      
      inFile <- input$boundFile2
      
      unzip(zipfile = inFile$datapath, exdir = temp.folder) #specify user poly here
      
      shp.file.dir <- list.files(path = temp.folder, pattern = "\\.shp$")
      shx.file.dir <- list.files(path = temp.folder, pattern = "\\.shx$")
      prj.file.dir <- list.files(path = temp.folder, pattern = "\\.prj$")
      dbf.file.dir <- list.files(path = temp.folder, pattern = "\\.dbf$")
      
      if(length(shp.file.dir) != 0 &
         length(shx.file.dir) != 0 &
         length(prj.file.dir) != 0 &
         length(dbf.file.dir) != 0){
      shp.layer <- strsplit(shp.file.dir, ".shp")

      poly <- readOGR(dsn = path.expand(paste0(temp.folder,"/",shp.file.dir)), layer=shp.layer[[1]])
      file.remove(paste0(temp.folder,"/",shp.file.dir))

      poly.trans <- gBuffer(spTransform(gUnionCascaded(poly), CRS("+init=epsg:3857")), width = 1)
      poly4map <- spTransform(gUnionCascaded(poly), CRS("+init=epsg:4326"))

      polyDF <- SpatialPolygonsDataFrame(poly.trans, data.frame(f=0), match.ID = F)

      writeOGR(polyDF, temp.folder, "userPoly", driver="ESRI Shapefile", overwrite_layer = T)
      withProgress(message = "Rendering user polygon",
                   value = 0.75,
                   leafletProxy('leaf') %>%
                     clearShapes() %>%
                     addPolygons(data = poly4map,
                                 weight = 2,
                                 color = '#317873',
                                 options =  pathOptions(pane = "tilePane", zIndex = 1000)))
      } else {
        shinyalert(title = 'Problem with user polygon!',
                   text = 'Please ensure that your upload contains the required files:<br>
                   .shp<br>
                   .shx<br>
                   .prj<br>
                   .dbf<br>',
                   type = 'warning',
                   closeOnClickOutside = TRUE,
                   html = T)
        NULL
      }
    }
  })
  
  accessionDF <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading Accession Data", value = 0.25)

    infile <- input$accession

    if (!is.null(infile)) {
      infile.dat <- as.data.frame(read.csv(infile$datapath, header=TRUE))
      colnames(infile.dat) <- c('id','x','y')
      infile.dat$id <- factor(infile.dat$id, levels = infile.dat$id)
      rasLoc <- path.expand('./climateMerc.tif')
      extract <- withProgress(message = "Extracting accession climate data",
                              value = 0.75,
                              matrix(gdallocationinfo(rasLoc,
                                                      coords = as.matrix(infile.dat %>% dplyr::select(x,y)),
                                                      wgs84 = T,
                                                      raw_output = T,
                                                      valonly = T),
                                     ncol = 7,
                                     nrow = nrow(infile.dat),
                                     byrow = T))
      climAttached <- data.frame(infile.dat, extract, stringsAsFactors = F)
      colnames(climAttached)[4:10] <- c("MAT","DiurnalRange","TSeasonality",
                                        "TWettestQtr","MAP","PSeasonality","PWarmestQtr")
      climAttached[,-1] <- sapply(climAttached[,-1], as.numeric) %>% as.data.frame()
      climAttached$MAT <- climAttached$MAT/10
      climAttached$DiurnalRange <- climAttached$DiurnalRange/10
      climAttached$TWettestQtr <- climAttached$TWettestQtr/10
      climAttached
    }else{
      NULL
    }
  })
  
  observe({
    df <- req(accessionDF())
    
    withProgress(message = "Rendering user polygon",
                 value = 0.75,
                 leafletProxy('leaf') %>%
                   clearMarkers() %>%
                   addCircleMarkers(data=accessionDF(), lng= ~x, lat =~y, 
                                    radius=6, color='white',fillOpacity = 1, stroke = F, 
                                    group="Overlays", label=~id) %>%
                   addCircleMarkers(data=accessionDF(), lng= ~x, lat =~y,
                                    radius=4, color= palettes(),fillOpacity = 1, stroke = F, 
                                    group="Overlays", label=~id ) %>%
                   addLegend("topright", pal = colorFactor(palettes(), 
                                                           domain=factor(accessionDF()$id),
                                                           na.color = 'transparent'),
                             values = accessionDF()$id,
                             title ="Accession",
                             layerId = 'assignL')) 
      
  })
  
  observeEvent(input$goButton,{
    if(is.null(accessionDF())) {
      shinyalert(title = 'No accession data uploaded!',
                 text = HTML('<b>Please upload a .csv file in the following format:</b><br><br>
                             <img src="https://storage.googleapis.com/seedmapper_dat/df.example.png", height = "48", width = "200"</img>'
                 ),
                 type = 'warning',
                 closeOnClickOutside = TRUE,
                 html = T)
    }
  })

  climClip <- eventReactive(input$goButton,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Extracting climate data for user extent", value = 0.1)
    
    req(accessionDF())
    
    if(input$boundSelect == "slider"){ 
      ext <- extent(input$lon.range[1],
                    input$lon.range[2],
                    input$lat.range[1],
                    input$lat.range[2])
      
      extPoly <- as(ext, "SpatialPolygons")
      sp::proj4string(extPoly) <- "+init=epsg:4326"
      extMercator <- spTransform(extPoly, CRS("+init=epsg:3857"))
      
      polyDF <- SpatialPolygonsDataFrame(extMercator, data.frame(f=0), match.ID = F)
      
      writeOGR(polyDF, temp.folder, "userPoly", driver="ESRI Shapefile", overwrite_layer = T)
      
      shpLoc <- path.expand(paste0(temp.folder,'/userPoly.shp'))
      rasLoc <- path.expand('./climateMerc.tif')
      
      gdalwarp(srcnodata=-9999, 
               dstnodata=-9999,
               overwrite = T,
               crop_to_cutline=T, 
               cutline = shpLoc,
               rasLoc,
               'clipped.tif')
      
      file.remove(paste0(temp.folder,'/userPoly.shp'))
      stack('clipped.tif')
    } else if
    
    (input$boundSelect == "poly" &
       !is.null(input$boundFile2)){
      
      shpLoc <- path.expand(paste0(temp.folder,'/userPoly.shp'))
      rasLoc <- path.expand('./climateMerc.tif')
      getwd()
      gdalwarp(srcnodata=-9999, 
               dstnodata=-9999,
               overwrite = T,
               crop_to_cutline=T, 
               cutline = shpLoc,
               rasLoc,
               'clipped.tif')
      stack('clipped.tif')
    } else {
      shinyalert(title = 'Invalid polygon!',
                 text = HTML('<b>Please upload a valid spatial polygon<b>'
                 ),
                 type = 'warning',
                 closeOnClickOutside = TRUE,
                 html = T)
    }
  })
  
  unscaled <- eventReactive(input$goButton,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Formatting selection", value = 0.15)
    
    req(climClip())
    clip <- climClip()
    
    clipMat <- as.matrix(clip)
    cell <- 1:ncell(clip)
    
    xy <- xyFromCell(clip, cell)
    roiDF <- na.omit(cbind(cell, xy[,1], xy[,2], clipMat))
    
    colnames(roiDF) <- c("cell", "x", "y", "MAT","DiurnalRange","TSeasonality",
                         "TWettestQtr","MAP","PSeasonality","PWarmestQtr")
    
    roiDF[,'MAT'] <- roiDF[,'MAT']/10
    roiDF[,'DiurnalRange'] <- roiDF[,'DiurnalRange']/10
    roiDF[,'TWettestQtr'] <- roiDF[,'TWettestQtr']/10
    
    if(nrow(roiDF) == 0){
      shinyalert(title = 'Invalid region of interest!',
                 text = HTML('Selection contains insufficient land area. Limit selection to land masses
                             within the bounds of -168 to -52 degrees longitude and 7 to 83 degrees latitude.'),
                 html = T,
                 type = 'error')
      
      NULL
    }else{
      roiDF
    }
  })
  
  scaling <- eventReactive(input$goButton,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Scaling data", value = 0.25)
    req(unscaled())
    unsc <- unscaled()
    rawAccession <- cbind(rep(0,nrow(accessionDF())), accessionDF()[,-1]) %>% as.matrix()
    colnames(rawAccession) <- colnames(unsc)
    accessionsAdded <- rbind(rawAccession, unsc)
    
    croppedStack <- cbind(accessionsAdded[,1:3], scale(accessionsAdded[,4:10]))
    
    croppedStack[,4] <- croppedStack[,4]*input$wtMAT
    croppedStack[,5] <- croppedStack[,5]*input$wtDiurnal
    croppedStack[,6] <- croppedStack[,6]*input$wtTSeason
    croppedStack[,7] <- croppedStack[,7]*input$wtTWet
    croppedStack[,8] <- croppedStack[,8]*input$wtMAP
    croppedStack[,9] <- croppedStack[,9]*input$wtPSeason
    croppedStack[,10] <- croppedStack[,10]*input$wtPWarm
    
    croppedStack
  })
  
  scaledAccessions <- eventReactive(input$goButton,{
    accSelect <- scaling()[1:nrow(accessionDF()),] %>% as.data.frame()
    colnames(accSelect) <- colnames(accessionDF())
    accSelect$id <- as.data.frame(read.csv(input$accession$datapath, header=TRUE)) %>% dplyr::select(id)
    accSelect
  })
  
  map.crop <- eventReactive(input$goButton,{
    afterAccessionsInd <- nrow(accessionDF()) + 1
    scaling()[afterAccessionsInd:nrow(scaling()),]
  })
  
  max.find <- eventReactive(input$goButton,{

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating maximum climate distance", value = 0.3)

    cropped.stack <- scaling() #contemplate whether it makes since to set this datasource as accession inclusive
    set.seed(123)
    while.maxxer <- function(){

      dists <- sqrt((cropped.stack[,4] - mean(cropped.stack[,4]))^2 + (cropped.stack[,5] - mean(cropped.stack[,5]))^2 +
                      (cropped.stack[,6] - mean(cropped.stack[,6]))^2 + (cropped.stack[,7] - mean(cropped.stack[,7]))^2 +
                      (cropped.stack[,8] - mean(cropped.stack[,8]))^2 + (cropped.stack[,9] - mean(cropped.stack[,9]))^2 +
                      (cropped.stack[,10] - mean(cropped.stack[,10]))^2)

      max.dists <- max(dists)

      repeat{
        w.max <- cropped.stack[which.max(dists),]

        test.dists <- sqrt((cropped.stack[,4] - w.max[4])^2 + (cropped.stack[,5] - w.max[5])^2 +
                             (cropped.stack[,6] - w.max[6])^2 + (cropped.stack[,7] - w.max[7])^2 +
                             (cropped.stack[,8] - w.max[8])^2 + (cropped.stack[,9] - w.max[9])^2 +
                             (cropped.stack[,10] - w.max[10])^2)

        new.max <- max(test.dists)
        if(new.max > max.dists){
          max.dists <- new.max
          w.max <- cropped.stack[which.max(test.dists),]
        } else{
          break
        }
      }
      return(max.dists)
    }

    extent.max <- while.maxxer()
    extent.max
  })
  
  sim.calcs <- eventReactive(input$goButton,{
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Mapping climate similarity", value = 0.6)
    
    cropped.stack <- map.crop()
    extent.max <- max.find()
    accessions <- scaledAccessions()
    maps.clust.fun <- function(clim.vals){
      col.dat <- clim.vals 
      
      clim.dist.df <- function(col){  #Euclidean distance function
        
        euc <- sqrt((cropped.stack[,4] - col.dat[col,4])^2 +
                      (cropped.stack[,5] - col.dat[col,5])^2 + 
                      (cropped.stack[,6] - col.dat[col,6])^2 +
                      (cropped.stack[,7] - col.dat[col,7])^2 +
                      (cropped.stack[,8] - col.dat[col,8])^2 + 
                      (cropped.stack[,9] - col.dat[col,9])^2 +
                      (cropped.stack[,10] - col.dat[col,10])^2)
        return(euc)
      }
      
      
      euc.out <- do.call(cbind, lapply(FUN=clim.dist.df, X = 1:nrow(col.dat))) #Applying the distance function over the accessions
      colnames(euc.out) <- paste(1:nrow(col.dat))
      accession <- cbind(apply( euc.out, 1, which.min)) #Determining which accession is closest in climate space for a given cell
      clim.sim <- 1 - rowMins(euc.out)/extent.max
      
      out <- cbind(accession, clim.sim, cropped.stack[,1])
      colnames(out)[1:3] <- c("accession", "clim.sim","cell")
      return(out) #returning data frame of relevant data
    }
    
    map.vals <- maps.clust.fun(clim.vals = accessions) 
    map.vals
  })
  
  palettes <- reactive({
    
    req(accessionDF())
    
      palette.full <- c("#8B1117",
                        "#29D32A",
                        "#F743FB",
                        "#03ADC3",
                        "#F0A733",
                        "#6F1D68",
                        "#7B8BFA",
                        "#188D57",
                        "#1F3D46",
                        "#FA5B93",
                        "#C498C4",
                        "#F8651F",
                        "#4E5705",
                        "#B7755E",
                        "#283F85",
                        "#E028B0",
                        "#92C015",
                        "#0196DB",
                        "#A1AB60",
                        "#DC9AF8",
                        "#66BE9F",
                        "#317313",
                        "#B95D18",
                        "#A27811",
                        "#61410C",
                        "#107585",
                        "#9B005F",
                        "#E92725",
                        "#B62DC2",
                        "#6ECC4B",
                        "#F3606F",
                        "#CCB437",
                        "#622515",
                        "#7A2C8F",
                        "#98A008",
                        "#4CCBC4",
                        "#FF70B8",
                        "#7C9FF7",
                        "#393364",
                        "#C64704",
                        "#4F673D",
                        "#A96AD3",
                        "#CB533C",
                        "#8ED14A",
                        "#9FAEC1",
                        "#C9A551",
                        "#85D1A2",
                        "#C6548A",
                        "#524874",
                        "#653A35")
      
      palette <- palette.full[1:nrow(accessionDF())]
    
  })
  
  rasStack <- eventReactive(input$goButton,{
    
    cropped.stack <- map.crop()
    extent.max <- max.find()
    map.vals <- sim.calcs()
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Projecting rasters", value = 0.7)
    
    leaf.template <- raster(nrow=nrow(climClip()), ncol = ncol(climClip()),
                            xmn=xmin(climClip()), xmx= xmax(climClip()),
                            ymn=ymin(climClip()), ymx=ymax(climClip()),
                            resolution=c(927.6624, 927.6624),
                            crs="+init=epsg:3857")
    
    clim.ras <- leaf.template
    bound.ras <- leaf.template
    
    values(clim.ras)[map.vals[,3]] <- round(map.vals[,2]*100)
    values(bound.ras)[map.vals[,3]] <- as.integer(map.vals[,1])
    
    dataType(clim.ras) <- "INT1U"
    dataType(bound.ras) <- "INT1U"
   
    stack(clim.ras,bound.ras)
  })
  
  rasterPals <- eventReactive(input$goButton, {
    clim.ras <- rasStack()[[1]]
    bound.ras <- rasStack()[[2]]
    
    grays <- withProgress(value=0.8, message="Assigning Aesthetics",gray.colors(n=10, start = 1, end = 0, alpha = NULL))
    
    sim.pal <- withProgress(value=0.90, message="Assigning Aesthetics",colorNumeric(grays, minValue(clim.ras):maxValue(clim.ras), 
                                                                                    na.color = 'transparent'))
    
    ras.zone.pal <- withProgress(value=0.95, message="Assigning Aesthetics",colorFactor(palettes(), 
                                                                                        domain=factor(1:nrow(accessionDF())),
                                                                                        na.color = 'transparent'))
    c(sim.pal, ras.zone.pal)
  })
  
  observeEvent(input$goButton,{
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }

    cropped.stack <- isolate(map.crop())
    extent.max <- isolate(max.find())
    map.vals <- isolate(sim.calcs())
    clim.ras <- isolate(rasStack()[[1]])
    bound.ras <- isolate(rasStack()[[2]])
    sim.pal <- isolate(rasterPals()[[1]])
    ras.zone.pal <- isolate(rasterPals()[[2]])
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering map", value = 0.99)
    
    ext <- extent(clim.ras)
    extPoly <- as(ext, "SpatialPolygons")
    sp::proj4string(extPoly) <- "+init=epsg:3857"
    extLatLon <- extent(spTransform(extPoly, CRS("+init=epsg:4326")))
    
    leafletProxy("leaf")  %>%
      flyToBounds(lng1 = extLatLon[1], lng2 = extLatLon[2], lat1 = extLatLon[3], lat2 = extLatLon[4]) %>%
      clearMarkers() %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addCircleMarkers(data=accessionDF(), lng= ~x, lat =~y, 
                       radius=6, color='white',fillOpacity = 1, stroke = F,
                       group="Overlays", label=~id) %>%
      addCircleMarkers(data=accessionDF(), lng= ~x, lat =~y,
                       radius=4, color= palettes(),fillOpacity = 1, stroke = F,
                       group="Overlays", label=~id ) %>%
      addRasterImage(clim.ras, colors = sim.pal, opacity = 0.9, 
                     project=FALSE, maxBytes = 8 * 1024 * 1024, group="Overlays") %>%
      addRasterImage(bound.ras, colors = ras.zone.pal, opacity = 0.45,
                     project=FALSE, maxBytes = 8 * 1024 * 1024, group="Overlays") %>%
      addLayersControl(baseGroups = c("Light Basemap", "Terrain"),
                       overlayGroups = c("Overlays"),
                       options = layersControlOptions(collapsed = F),
                       position=ifelse(nrow(accessionDF()) > 30, "topleft", "topright")) %>%
      addLegend("topright", pal = colorFactor(palettes(), 
                                              domain=factor(accessionDF()$id),
                                              na.color = 'transparent'),
                values = accessionDF()$id,
                title ="Assignment",
                layerId = 'assignL') %>%
      addLegend(ifelse(nrow(accessionDF()) > 20, "bottomleft", "topright"),
                pal = sim.pal, values = minValue(clim.ras):maxValue(clim.ras),
                title ="Climate<br>Similarity", bins = 5,
                layerId = 'simL')
    
  })
  
  clickPop <- eventReactive(input$leaf_click,{
    req(climClip())
    
    click.xy <- SpatialPoints(coords = data.frame(input$leaf_click$lng, input$leaf_click$lat),
                              proj4string=CRS('+init=epsg:4326'))
    click.trans <- spTransform(click.xy, '+init=epsg:3857') 
    clickCell <- cellFromXY(climClip(), click.trans)
    check <-  clickCell %in% unscaled()[,1]

    if(isFALSE(check)){
      return()
    } else {
      clickCell
    }
    
  })
  
  observe({
    req(clickPop())
    
    clickCell <- clickPop()
    cellUnscaleVals <- unscaled()[which(unscaled()[,1] == clickCell), ] %>% t() %>% 
      as.data.frame() %>% rename(Long = x, Lat = y)
    cellUnscaleVals$Long <- round(input$leaf_click$lng, 5)
    cellUnscaleVals$Lat <- round(input$leaf_click$lat, 5)
    
    labeler <- function(x){
      out <- paste0("<b>", colnames(cellUnscaleVals)[x], "</b>", ": ", cellUnscaleVals[,x], "<br>")
      return(out)
    }
    climPops <- do.call(rbind, lapply(FUN=labeler, X=2:10))
    
    cellUnscaleVals <- scaling()[which(scaling()[,1] == clickCell), ] %>% t() %>% 
      as.data.frame() %>% select(MAT:PWarmestQtr)
    
    accessionScaled <- scaledAccessions() %>% select(MAT:PWarmestQtr)
    
    eucInputs <- rbind(cellUnscaleVals, accessionScaled)
    
    dists <- dist(eucInputs) %>% as.matrix() 
    dropFocal <- dists[-1,1] %>% as.data.frame()
    dropFocal$id <- accessionDF()$id
    rankOrd <- dropFocal %>% arrange(dropFocal[,1])
    rankOrd[,1] <- 1 - rankOrd[,1]/max.find()
    ranker <- function(x){
      out <- paste0("<b>",x,")</b> ", rankOrd[x,2]," (", round(rankOrd[x,1]*100), ")<br>")
      return(out)
    }
    
    rankPops <- do.call(rbind, lapply(FUN=ranker, X=1:nrow(rankOrd)))
    
    popOut <- c('<u><b>Properties</u></b><br>', climPops, '<br><u><b>Similarity Rankings</u></b><br>', rankPops)

    leafletProxy("leaf")  %>%
      clearPopups() %>%
      addPopups(lng=input$leaf_click$lng, lat=input$leaf_click$lat,
                popup=HTML(popOut))
  })
  
  output$accessionTable <- renderDataTable({
    raw <- accessionDF() %>% dplyr::rename(Latitude = y, Longitude = x)
    raw
  })
  
  box.react <- eventReactive(input$goButton,{
    map.vals <- sim.calcs()
    
    forPlot <- cbind(map.vals[,1], unscaled()[,4:10]) %>% as.data.frame()
    colnames(forPlot) <- c("accession", colnames(unscaled()[,4:10]))
    melt <- withProgress(message="Formatting for box and whisker plots", value=0.93,
                         melt(forPlot, id.vars = "accession", measure.vars = c(colnames(forPlot[2:8]))))
    melt$accession <- factor(melt$accession, levels = 1:nrow(accessionDF()))
    levels(melt$accession) <- levels(accessionDF()$id)
    melt
  })
  
  ggSelection <- reactive({
    req(box.react)
    
    if(nrow(accessionDF()) > 25){
      cols <- 2
    }else{
      cols <- 1
    }
    
    fixedPalette <- palettes()
    names(fixedPalette) <- levels(box.react()$accession)
    
    box <- ggplot(data=box.react() %>% filter(variable == input$ggVar1 | variable ==  input$ggVar2 | variable ==  input$ggVar3),
                  aes(x=accession, y=value, fill=accession))+
      theme_bw()+
      geom_boxplot()+
      scale_fill_manual(values=fixedPalette)+
      guides(fill= guide_legend(title="Accession\nAssignment", ncol= cols))+
      facet_wrap(~variable, scales= "free_y", ncol=1)+
      xlab("Accession Assignments")+
      ylab("")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            text = element_text(size = 18, face = 'bold'))
    
    withProgress(message="Generating box and whisker plots", value=0.97, box)
    
  })
  
  output$boxPlot <- renderPlot({
    ggSelection()
  })

  output$downloadData <- downloadHandler(

    filename = paste("climate_partitioning_data_",Sys.Date(),".zip", sep=""),
    content = function(fname) {
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())

      old.wd <- getwd()
      setwd(temp.folder)
      
      fs <- c("simval.tif", "accession.assignment.tif", "accession.data.csv","assignment.val.key.csv",
              "metadata.txt","assignments.boxplot.pdf")

      map.vals <- isolate(sim.calcs())
      acc.dat <- isolate(accessionDF())
      
      fixedPalette <- palettes()
      names(fixedPalette) <- levels(box.react()$accession)
      
      if(nrow(accessionDF()) > 25){
        cols <- 2
      }else{
        cols <- 1
      }
      
      box <- ggplot(data=box.react(), aes(x=accession, y=value, fill=accession))+
        theme_bw()+
        geom_boxplot()+
        scale_fill_manual(values=fixedPalette)+
        guides(fill= guide_legend(title="Accession\nAssignment", ncol= cols))+
        facet_wrap(~variable, scales= "free_y", ncol=1)+
        xlab("Accession Assignments")+
        ylab("")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size = 14, face = 'bold'))
      
      withProgress(message = "Saving box plot data", value = 0.5, 
                   ggsave(filename=paste0(temp.folder,"/assignments.boxplot.pdf"), 
                          plot=box, width=8.5,height=15, dpi=300))

      fac.lvls <- data.frame(accessionDF()$id, 1:nrow(accessionDF()))
      colnames(fac.lvls) <- c("Accession","Raster Value")

      progress$set(message = "Rasterizing Data", value = 0.75)
      
      clim.ras <- rasStack()[[1]]
      bound.ras <- rasStack()[[2]]
      
      projClim.ras <- projectRaster(clim.ras, crs = '+init=epsg:4326')
      projBound.ras <- projectRaster(bound.ras, crs = '+init=epsg:4326', method = 'ngb')
      NAvalue(projClim.ras) <- 0
      NAvalue(projBound.ras) <- 0
      
      progress$set(message = "Writing Raster Files", value = 0.95)
      
      writeRaster(projClim.ras, "simval.tif", format="GTiff", overwrite=TRUE, datatype = "INT1U", NAflag = 0)
      writeRaster(projBound.ras,"accession.assignment.tif", format="GTiff", overwrite=TRUE, datatype = "INT1U", NAflag = 0)

      write.csv(acc.dat, file = "accession.data.csv", row.names = FALSE)
      write.csv(fac.lvls, file = "assignment.val.key.csv", row.names = FALSE)

      cat("Explanation of contents:",
          "",
          "simval.tif: this raster contains the climate similarity values for the extent of interest. It reports how climatically close each cell is to its accession assignment",
          "",
          "accession.assignment.tif: this raster contains values of accession assignments for all cells within the extent of interest",
          "",
          "accession.data.csv: this file contains coordinates and climate variate values for all accessions analyzed",
          "",
          "assignment.val.key.csv: this file contains the integer value associated with each accession in the accession.assignment.tif",
          "",
          "assignments.boxplot.pdf: this plot displays the distribution of climate data for the cells within each accession assignment ",
          "",
          "Symbology instructions:",
          "",
          "Set high values of the simval.tif as black and low values as white. Overlay accession.assignment.tif and set it to 50% transparency with values as categorical, each a unique color"
          , file="metadata.txt",sep="\n")

      print (fs)

      progress$set(message = "Compressing Files", value = 0.9)
      zip(zipfile=fname, files=fs)
      setwd(old.wd)
    },
    contentType = "application/zip"
  )


})