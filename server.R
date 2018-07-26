library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(rsconnect)
library(shiny)
library(devtools)
library(RCurl)
library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
library(leaflet)
library(htmlwidgets)

options(shiny.maxRequestSize=30*1024^2) 

na.template <- raster(nrow=5400, ncol = 4800, 
                      xmn=-130, xmx=-90, ymn=15, ymx=60, 
                      resolution=c(0.008333333, 0.008333333), 
                      crs="+proj=longlat +datum=WGS84")

leaf.template <- raster(nrow=5400, ncol = 4800, 
                        xmn=-14471534, xmx=-10018754, ymn=1689200, ymx=8399738, 
                        resolution=c(927.6624, 1242.692), 
                        crs=" +init=epsg:3857")


temp.folder <- tempdir()
#download.file("https://s3-us-west-1.amazonaws.com/partition.app.tw/na.cells.csv.zip", destfile=temp.file, method="curl")
unzip(zipfile = "na.cells.csv.zip", exdir = temp.folder)
na.pts <- fread(paste(temp.folder,"/na.cells.csv",sep=""),data.table = FALSE)

shinyServer(function(input, output, session) {

  output$loading <- renderUI({
    if(is.null(na.pts)){
      helpText("App takes ~15 seconds to initialize, please wait.")
    }else{
      helpText(tags$b("App ready!"))
    }
  })
  
  accession.df <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading Accession Data", value = 0.1)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    infile <- input$accession.csv
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return()
    }else{
    infile.dat <- as.data.frame(read.csv(infile$datapath, header=TRUE))
    infile.cell <- data.frame(cellFromXY(na.template, infile.dat[,2:3]))
    colnames(infile.cell) <- "cell"
    
    infile.cell.ord <- infile.cell[order(infile.cell$cell),]
    
    merge(infile.cell, na.pts, by="cell")}
    
  })
  
  accession.names <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Naming accesssions", value = 0.15)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    if(is.null(accession.df())){
      return()
    }else{
    
    acc.dat <- accession.df()
    
    infile <- input$accession.csv
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return()
    }
    infile.dat <- as.data.frame(read.csv(infile$datapath, header=TRUE))
    name <- infile.dat[,1] 
    cell <- cellFromXY(na.template, infile.dat[,2:3])
    prep <- data.frame(cell,name)
    colnames(prep) <- c("cell","name")
    
    sub <- subset(prep, prep$cell %in% acc.dat$cell)
    sub.sort <- sub[order(sub$cell),]
    data.frame(sub.sort$name)}
    
  })
  
  map.crop <- reactive({
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Extracting climate data for user extent", value = 0.2)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    isolate(if(input$boundSelect == "slider"){ #extent set by sliders
      box.crop  <- subset(na.pts, na.pts$x > input$lon.range[1] &  
                            na.pts$x < input$lon.range[2] &  
                            na.pts$y > input$lat.range[1] &
                            na.pts$y < input$lat.range[2])
      cropped.stack <- box.crop
    })
    
    isolate(if(input$boundSelect == "poly"){ #extent set by poly
      
      temp.folder <- tempdir()
      inFile <- input$boundFile2
      unzip(zipfile = inFile$datapath, exdir = temp.folder) #specify user poly here
      shp.file.dir <- list.files(path = temp.folder, pattern = "\\.shp$")
      shp.layer <- strsplit(shp.file.dir, ".shp")[[1]]
      
      poly <- readOGR(dsn = paste(temp.folder,"/",shp.file.dir, sep=""), layer=shp.layer)
      file.remove(paste(temp.folder,"/",shp.file.dir, sep=""))
      
      poly.trans <- spTransform(poly, CRS("+proj=longlat +datum=WGS84"))
      
      poly.cell <- data.frame(cellFromPolygon(na.template, poly.trans))
      
      poly.crop <- subset(na.pts, na.pts$cell %in% poly.cell[,1])
      cropped.stack <- poly.crop
    })
    
    isolate(if(input$boundSelect == "raster"){ #extent set by raster
      user.ras <- input$boundFile1
      raster.pts <- as.data.frame(rasterToPoints(raster(user.ras$datapath))) #specify user raster here
      raster.cells <- unique(cellFromXY(na.template, raster.pts[,1:2]))
      raster.crop <- subset(na.pts, na.pts$cell %in% raster.cells)
      cropped.stack <- raster.crop
    })
    
    isolate(if(input$boundSelect == "sdm"){ #extent set by sdm
      sp <- paste(input$sdm.select, ".cell.csv", sep="")
      
      download.file(paste("https://s3-us-west-1.amazonaws.com/seed.match.tw/", sp,
                          ".zip", sep=""), destfile=temp.file, method="curl")
      unzip(zipfile = temp.file, exdir = temp.folder)
      sp.cell <- fread(paste(temp.folder,"/", sp ,sep=""), data.table = FALSE)
      
      sdm.crop  <- subset(na.pts, na.pts$cell %in% sp.cell$cell)
      cropped.stack <- sdm.crop
    })
    
    colnames(cropped.stack) <- colnames(na.pts)
    cropped.stack
    
  }) #verified good
  
  rank <- reactive({
    
    acc.dat <- accession.df()
    acc.names <- accession.names()
    acc.bind <- data.frame(acc.names,acc.dat)
    map.crop <- map.crop()
    colnames(acc.bind)[1] <- "Accession"
    
    if(is.null(input$leafmap_click$lng) |
       input$leafmap_click$lng < -130 |
       input$leafmap_click$lng > -90 |
       input$leafmap_click$lat < 15 |
       input$leafmap_click$lat > 60 &
       input$clickButton == "clim"
       ){return()}else{
    
    rank.pt.cell <- cellFromXY(na.template, data.frame(input$leafmap_click$lng,input$leafmap_click$lat))
    rank.pt.dat <- data.frame("rank.pt", subset(na.pts, na.pts$cell %in% rank.pt.cell))
    colnames(rank.pt.dat) <- colnames(acc.bind)
    
    rank.bind <- rbind(rank.pt.dat, acc.bind)
    crop.notin <- data.frame("not.in",subset(map.crop, !(map.crop$cell %in% rank.bind$cell)))
    colnames(crop.notin) <- colnames(acc.bind)
    scale.bind <- rbind(rank.bind, crop.notin)
    scale.bind[,5:11] <- scale(scale.bind[,5:11])
    
    rank.dist <- as.matrix(dist(scale.bind[1:nrow(rank.bind),5:11]), method="euclidean")
    name.dist <- data.frame(acc.names, rank.dist[2:nrow(rank.dist),1])
    name.dist.ord <- name.dist[order(name.dist[,2]),]
    
    rank.list <- data.frame(1:nrow(name.dist.ord),name.dist.ord[,1])
    colnames(rank.list) <- c("Rank","Accession")
    rank.list}
    
  })
  
  acc.scale <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating maximum climate distance", value = 0.3)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    acc.dat <- isolate(accession.df())
    cropped.stack <- isolate(map.crop())
    
    acc.bound <- rbind(acc.dat, cropped.stack)
    scaled <- data.frame(acc.bound[,1:3], scale(acc.bound[,4:10]))
    scaled[1:nrow(acc.dat),]
    
  })
  
  map.clean <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating maximum climate distance", value = 0.35)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    acc.dat <- isolate(accession.df())
    cropped.stack <- isolate(map.crop())
    
    bind <- rbind(acc.dat, cropped.stack)
    scale.bind <- data.frame(bind[,1:3], scale(bind[,4:10]))
    unique(subset(scale.bind, scale.bind$cell %in% cropped.stack$cell))
    
  })
  
  max.find <- reactive({
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating maximum climate distance", value = 0.3)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    m.clean <- isolate(map.clean())
    acc <- isolate(acc.scale())
    cropped.stack <- rbind(acc,m.clean)
    
    
    while.maxxer <- function(){
      
      dists <- sqrt((cropped.stack[,4] - mean(cropped.stack[,4]))^2 + (cropped.stack[,5] - mean(cropped.stack[,5]))^2 + 
                      (cropped.stack[,6] - mean(cropped.stack[,6]))^2 + (cropped.stack[,7] - mean(cropped.stack[,7]))^2 +
                      (cropped.stack[,8] - mean(cropped.stack[,8]))^2 + (cropped.stack[,9] - mean(cropped.stack[,9]))^2 +
                      (cropped.stack[,10] - mean(cropped.stack[,10]))^2)
      
      max.dists <- max(dists)
      
      repeat{
        w.max <- cropped.stack[which.max(dists),]
        
        test.dists <- sqrt((cropped.stack[,4] - w.max[,4])^2 + (cropped.stack[,5] - w.max[,5])^2 + 
                             (cropped.stack[,6] - w.max[,6])^2 + (cropped.stack[,7] - w.max[,7])^2 +
                             (cropped.stack[,8] - w.max[,8])^2 + (cropped.stack[,9] - w.max[,9])^2 +
                             (cropped.stack[,10] - w.max[,10])^2)
        
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
  }) #verified good
  
  sim.calcs <- reactive({
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Mapping climate similarity", value = 0.7)
    
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    cropped.stack <- isolate(map.clean())
    extent.max <- isolate(max.find())
    acc.scale <- isolate(acc.scale())
    acc.names <- isolate(accession.names())
    
    maps.clust.fun <- function(clim.vals){
      col.dat <- clim.vals # defining our data frame of accessions 
      
      clim.dist.df <- function(col){  #Euclidean distance function
        
        euc <- sqrt((cropped.stack[,4] - col.dat[col,4])^2 + (cropped.stack[,5] - col.dat[col,5])^2 + 
                      (cropped.stack[,6] - col.dat[col,6])^2 + (cropped.stack[,7] - col.dat[col,7])^2 +
                      (cropped.stack[,8] - col.dat[col,8])^2 + (cropped.stack[,9] - col.dat[col,9])^2 +
                      (cropped.stack[,10] - col.dat[col,10])^2)
        return(euc)
      }
      
      euc.out <- as.data.frame(do.call(cbind, lapply(FUN=clim.dist.df, X = 1:nrow(col.dat)))) #Applying the distance function over the accessions
      colnames(euc.out) <- acc.names[,1]
      zone <- cbind(apply( euc.out, 1, which.min)) #Determining which accession is closest in climate space for a given cell
      zone.namer <- function(x){ # a function to rename the indices from zone to the corresponding accession id
        return(colnames(euc.out)[x])
      }
      accession <- do.call(rbind, lapply(FUN=zone.namer, X=zone)) # applying zone.namer across all accessions
      clim.sim <- 1- as.data.frame(do.call(pmin, euc.out))/extent.max #identifying minimum climate distance by cell, converting to climate similarity
      
      
      out <- as.data.frame(cbind(accession, clim.sim, cropped.stack)) #georeferencing the closest accession and climate similarity data
      colnames(out)[2] <- "clim.sim"
      return(out) #returning data frame of relevant data
    }
    
    map.vals <- maps.clust.fun(clim.vals = acc.scale)
    #levels(map.vals$accession) <- levels(medoid.print$`Climate Center`)
    #map.vals$accession <- factor(map.vals$accession, levels= paste("Center",1:nrow(medoid.print)))
    map.vals
  }) 
  
  box.react <- reactive({
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    map.vals <- isolate(sim.calcs())
    acc.dat <- isolate(accession.df())
    acc.names <- isolate(accession.names())
    acc.bind <- data.frame(acc.names,acc.dat)
    colnames(acc.bind)[1] <- "Accession"
    
    sub.vals <- withProgress(message="Extracting data for center assignments", value=0.91, 
                             subset(na.pts, na.pts$cell %in% map.vals$cell))
    forPlot <- cbind(map.vals$accession,sub.vals[,4:10])
    colnames(forPlot) <- c("accession", colnames(na.pts[,4:10]))
    forPlot$MAT <- forPlot$MAT/10
    forPlot$DiurnalRange <- forPlot$DiurnalRange/10
    forPlot$TWettestQtr <- forPlot$TWettestQtr/10
    melt <- withProgress(message="Formatting for box and whisker plots", value=0.93,
                         melt(forPlot, id.vars = "accession", measure.vars = c(colnames(forPlot[2:8]))))
    
    if(nrow(acc.dat) <= 50){
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
      
      palette <- palette.full[1:nrow(acc.dat)]
    }
    
    if(nrow(acc.dat) > 25){
      cols <- 2
    }else{
      cols <- 1
    }
    
    melt$accession <- factor(melt$accession, levels = acc.bind$Accession)
    
    box <- ggplot(data=melt, aes(x=accession, y=value, fill=accession))+
      theme_bw()+
      geom_boxplot()+
      scale_fill_manual(values=palette)+
      guides(fill= guide_legend(title="Accession \nAssignment", ncol= cols))+
      facet_wrap(~variable, scales= "free_y", ncol=1)+
      xlab("Accession Assignments")+
      ylab("")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    withProgress(message = "Saving box plot data", value = 0.95, ggsave(filename=paste(temp.folder,"/assignments.boxplot.pdf",sep=""), plot=box, width=8.5,height=11, dpi=300))
    withProgress(message="Generating box and whisker plots", value=0.97, box)
  })
  
  output$leafmap <- renderLeaflet({
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    cropped.stack <- isolate(map.crop())
    extent.max <- isolate(max.find())
    map.vals <- isolate(sim.calcs())
    acc.dat <- isolate(accession.df())
    acc.names <- isolate(accession.names())
    acc.bind <- data.frame(acc.names,acc.dat)
    colnames(acc.bind)[1] <- "Accession"
    
    map.sp <- SpatialPointsDataFrame(cbind(map.vals$x,map.vals$y),map.vals, proj4string =CRS("+proj=longlat +datum=WGS84"))
    
    map.sp.lf <- spTransform(map.sp, CRSobj = crs(leaf.template))
    
    names.reduced <- factor(unique(map.vals$accession))
    names.ord <- acc.bind[acc.bind$Accession %in% names.reduced,]
    names.ord$Accession <- as.character(names.ord$Accession)
    
    map.vals$accession <- factor(map.vals$accession, levels=acc.bind$Accession)
    plyr::mapvalues(map.vals$accession, from = levels(map.vals$accession), to = 1:length(levels(map.vals$accession)))

    clim.ras <- withProgress(value=0.8, message="Projecting Rasters", rasterize(map.sp.lf, leaf.template, field=map.vals$clim.sim))
    bound.ras <- withProgress(value=0.85, message="Projecting Rasters",rasterize(map.sp.lf, leaf.template, field=as.numeric(map.vals$accession)))
    
   
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
      
      palette <- palette.full[1:length(names.reduced)]
    
    
    grays <- withProgress(value=0.87, message="Assigning Aesthetics",gray.colors(n=10, start = 1, end = 0, alpha = NULL))
    
    sim.pal <- withProgress(value=0.89, message="Assigning Aesthetics",colorNumeric(grays, values(clim.ras),
                            na.color = "transparent"))
    
    ras.zone.pal <- withProgress(value=0.9, message="Assigning Aesthetics",colorFactor(palette, 
                            domain=factor(values(bound.ras)),
                            na.color = "transparent"))
    
    labs <- factor(names.ord$Accession, levels = names.ord$Accession)
    
    leg.zone.pal <- withProgress(value=0.9, message="Assigning Aesthetics",colorFactor(palette, 
                                domain=labs,
                                na.color = "transparent"))
    
      m <- leaflet() %>% 
        addProviderTiles("CartoDB.Positron", group="Light Basemap") %>%
        addProviderTiles("Esri.WorldTopoMap", group="Terrain") %>%
        addRasterImage(clim.ras, colors = sim.pal, opacity = 0.8, project=FALSE, maxBytes = 6 * 1024 * 1024, group="Assignments") %>%
        addRasterImage(bound.ras, colors = ras.zone.pal, opacity = 0.4, project=FALSE, maxBytes = 6 * 1024 * 1024, group="Assignments") %>%
        addCircleMarkers(data=acc.bind, lng= ~x, lat =~y, radius=4, color="black", group="Accessions") %>%
        addCircleMarkers(data=acc.bind, lng= ~x, lat =~y, radius=3, color="white", 
                         fillOpacity = 1,  group="Accessions", label=~acc.bind$Accession) %>%
        addLegend("bottomleft",pal = sim.pal, values = values(clim.ras),
                  title ="Climate Similarity") %>%
        addLegend("topright",pal = leg.zone.pal,
                  values = labs,
                  title ="Assignment") %>%
        addLayersControl(baseGroups = c("Light Basemap", "Terrain"),
                         overlayGroups = c("Assignments", "Accessions"),
                         options = layersControlOptions(collapsed = FALSE),
                         position="topleft") 
    
    withProgress(value=0.95, message="Rendering Map",m)
    
  })
  
  click.list <- reactive({
    if(!is.null(rank()) & input$clickButton == "rank"){
    
    rank <- rank()$Accession
    ranker <- function(x){
      paste(rank[x], "<br>")
    }
  do.call(rbind, lapply(FUN=ranker, X=1:length(rank)))
    }
    
    else{
      map.vals <- sim.calcs()
      cropped.stack <- map.crop()
      
      if(is.null(input$leafmap_click$lng) |
         input$leafmap_click$lng < -130 |
         input$leafmap_click$lng > -90 |
         input$leafmap_click$lat < 15 |
         input$leafmap_click$lat > 60
      ){return()}else{
        
        map.cell <- cellFromXY(na.template, data.frame(input$leafmap_click$lng,input$leafmap_click$lat))
      }
      
      center <- subset(map.vals, cell == map.cell, select=c("accession"))
      vals <- subset(na.pts, cell == map.cell, select=c("MAT","DiurnalRange","TSeasonality",
                                                        "TWettestQtr","MAP","PSeasonality","PWarmestQtr"))
      
      if(nrow(center) > 0){
        sub <- data.frame(center,vals)
        
        sub$MAT <- sub$MAT/10
        sub$DiurnalRange <- sub$DiurnalRange/10
        sub$TWettestQtr <- sub$TWettestQtr/10
        
        colnames(sub)[1] <- c("Assignment")
        labeler <- function(x){
          out <- paste("<b>", colnames(sub)[x], "</b>", ":", sub[,x], "<br>")
          return(out)
        }
        
        out <- do.call(rbind, lapply(FUN=labeler, X=1:ncol(sub)))
      }else{out <- NULL}
      out
    }
  
  })
  
  observe({
    
    if(!is.null(input$leafmap_click$lng) && !is.null(click.list()) && input$clickButton =="rank"){
    leafletProxy("leafmap")  %>% 
      clearPopups() %>%
      addPopups(lng=input$leafmap_click$lng, lat=input$leafmap_click$lat,
                popup=paste("<b>", "Rankings at","<br>",
                                  input$leafmap_click$lng, "Longitude", "<br>",
                                  input$leafmap_click$lat, "Latitude", "<br>", "</b>",
                                  paste(click.list(), collapse="")))
      }else if
    
    (!is.null(input$leafmap_click$lng) && !is.null(click.list()) && input$clickButton =="clim"){
      leafletProxy("leafmap")  %>% 
        clearPopups() %>%
        addPopups(lng=input$leafmap_click$lng, lat=input$leafmap_click$lat,
                  popup=paste(
                    "<b>", "Long:", "</b>", input$leafmap_click$lng,  "<br>",
                    "<b>", "Lat:","</b>", input$leafmap_click$lat,  "<br>", 
                    paste(click.list(), collapse="")))}
    
  })
  
  
  output$condPanel <- renderUI({
    if(!is.null(sim.calcs()) && input$tabs1 == 'Map'){
      absolutePanel(top="405px", left="260px", width=110, height=80,
                                   wellPanel(style="opactiy: 0.92; font-size: 12px",
                                     radioButtons(inputId="clickButton",
                                                label="Click for:",
                                                choices = list(
                                                  "Rankings" = "rank",
                                                  "Climate Data" = "clim"
                                                ))))
    }
  })
  
  output$boxPlot <- renderPlot({
    box.react()
  })
  
  acc.prep <- reactive({
  
    acc.dat <- accession.df()
    acc.names <- accession.names()
    acc.bind <- data.frame(acc.names,acc.dat)
    colnames(acc.bind)[1] <- "Accession"
    
    acc.bind$MAT <- acc.bind$MAT/10
    acc.bind$DiurnalRange <- acc.bind$DiurnalRange/10
    acc.bind$TWettestQtr <- acc.bind$TWettestQtr/10
    acc.bind
    
  })
  
  output$centerTable <- renderDataTable({
    if(!is.null(acc.prep)){
    acc.prep()}
  })

  output$downloadData <- downloadHandler(
    
    filename = paste("climate_partitioning_data_",Sys.Date(),".zip", sep=""),
    content = function(fname) {
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      old.wd <- getwd()
      setwd(temp.folder)
      print(temp.folder)
      
      map.vals <- isolate(sim.calcs())
      acc.dat <- isolate(accession.df())
      acc.names <- isolate(accession.names())
      acc.bind <- data.frame(acc.names,acc.dat)
      colnames(acc.bind)[1] <- "Accession"
      box.react <- isolate(box.react())
      
      names.reduced <- factor(unique(map.vals$accession))
      names.ord <- acc.bind[acc.bind$Accession %in% names.reduced,]
      names.ord$Accession <- as.character(names.ord$Accession)
      
      fac.lvls <- data.frame(names.ord$Accession, 1:length(names.ord$Accession))
      names(fac.lvls) <- c("Accession","Raster Value")
      
      fs <- c("simval.tif", "accession.assignment.tif", "accession.data.csv","assignment.val.key.csv",
              "metadata.txt","assignments.boxplot.pdf")
      
      progress$set(message = "Rasterizing Data", value = 0.25)
      
      map.vals$accession <- factor(map.vals$accession, levels=acc.bind$Accession)
      mapvalues(map.vals$accession, from = levels(map.vals$accession), to = 1:length(levels(map.vals$accession)))
      
      clim.ras <- rasterize(map.vals[,4:5], na.template, field=map.vals$clim.sim)
      bound.ras <- rasterize(map.vals[,4:5], na.template, field=as.numeric(map.vals$accession))
      
      progress$set(message = "Writing Raster Files", value = 0.5)
      
      writeRaster(clim.ras, "simval.tif", format="GTiff", overwrite=TRUE)
      writeRaster(bound.ras,"accession.assignment.tif", format="GTiff", overwrite=TRUE)
      
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