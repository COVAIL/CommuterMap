# csv <- read_csv("/Users/pgordon/ownCloud/Commuter Map/Geocoded_address.csv")
# csv <- csv$search
# csv <- csv[1:5]
# write.table(csv, file="input", row.names=F, col.names=F, quote=F)
# t <- read.table('/Users/pgordon/ownCloud/Commuter Map/Map_App/input', quote="", sep="\n")
# v <- as.vector(t$V1)
# r <- mp_geocode(v)
# pnt <- mp_get_points(r)
# df <- structure(dplyr::bind_cols(search = pnt$address, as.data.frame(do.call(rbind, pnt$pnt)), address = pnt$address_google), names = c("search", "lon", "lat", "address"))


library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(readr)
library(rgdal)
library(sp)
library(mapsapi)
library(future)
library(httr)
library(jsonlite)
library(shinyjs)
library(dplyr)

# USER INTERFACE
########################################


showLogos <- function() {
          return(
              fluidRow(
              fluidRow(
                column(8, align="center",
                       img(src='logo_cbuscollab.png', width="30%", height="30%"),
                       img(src='logo_ohiohealth.png', width="30%", height="30%")
                )
              ),
              fluidRow(
                column(8, align="center",
                       img(src='logo_smartcity.jpg', width="10%", height="10%", align="center")
                )
              )
              )
          )
}

ui <- dashboardPage(
  dashboardHeader(title="Commuter Map"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem("Commuter Map Tool", tabName="mapMenu", icon = icon("map"), startExpanded=TRUE,
               menuSubItem("Upload Data", icon = icon("upload"), tabName = "upload"),
               menuItemOutput("mapOutput"),
               menuItemOutput("mapDownloadOutput")
      ),
      menuItem("Geocode Tool", tabName = "geocodeMenu", icon = icon("dashboard"), startExpanded=TRUE, 
               menuSubItem("Geocode Upload Tool", tabName="geocode", icon = icon("upload"))
      ),
      menuItem("Anonymize Tool", tabName = "anonymizeMenu", icon = icon("dashboard"), startExpanded=TRUE, 
               menuSubItem("Anonymize Upload Tool", tabName="anonymize", icon = icon("upload"))
      ),
      menuItem("Merge Tool", tabName = "mergeMenu", icon = icon("dashboard"), startExpanded=TRUE, 
               menuSubItem("Merge Upload Tool", tabName="merge", icon = icon("upload"))
      )
      
    )
  ),
  dashboardBody(
    tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-126191069-1'></script>
                   <script>
                   window.dataLayer = window.dataLayer || [];
                   function gtag(){dataLayer.push(arguments);}
                   gtag('js', new Date());
                   
                   gtag('config', 'UA-126191069-1');
                   </script>
                   ")),
    tabItems(
      tabItem(tabName = "upload",
              fluidPage(
                fluidRow(# Input: Select a file ----
                         h2("Select a file"),
                         h5("The uploaded file must contain the Address, longitude and latitude columns on each row; and a header row."),
                         h5("The first record needs to be the reference address that you are mapping in relation to."),
                         h5("If you do not have geocoded addresses with longitude and latitude columns, use the geocode upload tool to get the geocoded address information to upload here."),
                         h5("If you want to see company addresses grouped you must upload a company column in the data."),
                         h5("The result is shown on the Interactive Map"), 
                         tags$ul(tags$li(a("Input Sample", href="samples/geocode_output.csv"))),
                         fileInput("fileUpload", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv"))
                         ),
                showLogos()
              )
      ),
      tabItem(tabName = "geocode",
              fluidPage(
                fluidRow(# Input: Select a file ----
                         h2("Select a file"),
                         h5("The uploaded file must contain an Address on each row; and no headers."),
                         h5("The result file will contain address, longitude and latitude columns.  You need to press the download button to download the result file, once the processing is complete."),
                         tags$ul(tags$li(a("Input Sample", href="samples/geocode_input.csv")),
                         tags$li(a("Ouput Sample", href="samples/geocode_output.csv"))),
                         fileInput("fileGeocode", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv"))
                ),
                fluidRow(
                  downloadButton('downloadGeocodedData') 
                ),
                showLogos()
              )
      ),
      tabItem(tabName = "anonymize",
              fluidPage(
                fluidRow(# Input: Select a file ----
                         h2("Select a file"),
                         h5("The uploaded file must contain an Address, lon, lat on each row; and no headers."),
                         h5("The result file will contain longitude and latitude columns.  You need to press the download button to download the result file, once the processing is complete."),
                         h5("Provide a distance in kilometers to randomly shift the longitude and latitude."),
                         tags$ul(tags$li(a("Input Sample", href="samples/anonymize_input.csv")),
                         tags$li(a("Ouput Sample", href="samples/anonymize_output.csv"))),
                         textInput("namedGeocode", "Name to associate with geocodes:"),
                         numericInput("distance", "Distance to Anonymize (kilometers):", .1, min = 0, max = 1),
                         fileInput("fileAnonymize", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv"))
                ),
                fluidRow(
                  downloadButton('downloadAnonymizedData')
                ),
                showLogos()
              )
      ), 
      tabItem(tabName = "merge",
              fluidPage(
                fluidRow(# Input: Select a file ----
                         h2("Select a file"),
                         h5("You must fill in the NAME before uploading the file."),
                         h5("The uploaded file must contain the Address, longitude and latitude columns on each row; and a header row."),
                         tags$ul(tags$li(a("Input Sample \"Innovation\"", href="samples/merge_input_innovation.csv")),
                         tags$li(a("Input Sample \"Digital Transformation\"", href="samples/merge_input_digitaltransformation.sv")),
                         tags$li(a("Ouput Sample", href="samples/merge_output.csv"))),
                         textInput("companyMerge1", "Name to associate with addresses:"),
                         fileInput("fileMerge1", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv")),
                         textInput("companyMerge2", "Name to associate with addresses:"),
                         fileInput("fileMerge2", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv")),
                         textInput("companyMerge3", "Name to associate with addresses:"),
                         fileInput("fileMerge3", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv")),
                         textInput("companyMerge4", "Name to associate with addresses:"),
                         fileInput("fileMerge4", NULL, multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain",
                                              ".csv"))
                ),
                fluidRow(
                  downloadButton('downloadMergedData')
                ),
                showLogos()
              )
      ),       
      tabItem(tabName="map",
              fluidPage(
                fluidRow(
                  leafletOutput("map", width = "100%", height = "725")
                )
              )
      ),
      tabItem(tabName="download",
              fluidPage(
                fluidRow(
                  downloadButton('downloadReport') 
                )
              ))
    ) 
  )
)



createLeaflet <- function(df, Mapstuff){
  
  #return(usingPipes(df, Mapstuff))
  return(notUsingPipes(df, Mapstuff))
   

}

notUsingPipes <- function(Coded, Mapstuff){
  
   definedBaseGroups <- c("ESRI", "OpenStreetMap", "Satellite")
   definedOverlayGroups <- c("Reference", "Park & Ride", "COTA Stops", 
                             "1 Mile Radius", "5 Mile Radius", "10 Mile Radius", 
                             "20 Mile Radius", "30 Mile Radius")
   my_map <- leaflet()
   my_map <- setView(map=my_map, lng = -82.9988, lat = 39.9612, zoom = 10)
   my_map <- addProviderTiles(map=my_map, providers$Esri.WorldStreetMap, group = "ESRI")
   my_map <- addTiles(map=my_map, group = "OpenStreetMap")
   my_map <- addProviderTiles(map=my_map, providers$Esri.WorldImagery, group = "Satellite")
   my_map <- addMeasure(my_map, position = "topleft", primaryLengthUnit = "miles", primaryAreaUnit = "sqmiles")
   
   if("company" %in% names(Coded)){
     Coded$companyFactor <- factor(Coded$company)
     factpal <- colorFactor(topo.colors(length(unique(Coded$company))), Coded$companyFactor)
     #for each unique company give it a different color
     #set label to be company
     my_map <- addCircleMarkers(my_map, data = Coded[-1, ], lat = ~lat, lng = ~lon, label = ~company,
                                popup = ~company, color = ~factpal(companyFactor),
                                group = ~company)  
     for(c in unique(Coded$company)){
       definedOverlayGroups <- c(definedOverlayGroups, c)
       #my_map <- hideGroup(my_map, c)  
     }
     
   }else{
     my_map <- addCircleMarkers(my_map, data = Coded[-1, ], lat = ~lat, lng = ~lon, label = ~address,
                                popup = ~address, color = "green",
                                clusterOptions = markerClusterOptions())     
   }
   
   my_map <- addMarkers(my_map, data = Mapstuff()$PR2, lng = ~lon, lat = ~lat,
               label = ~name, group = "Park & Ride",
               icon = Mapstuff()$Pnr_icon)
   my_map <-    addMarkers(my_map, data = Mapstuff()$Stops, lng = ~lon, lat = ~lat,
                 label = ~stopname, group = "COTA Stops",
                 icon = Mapstuff()$Bus_icon)
   my_map <- addMarkers(my_map, data = Coded[1, ], lng = ~lon, lat = ~lat, label = ~address,
                 group = "Reference", icon = Mapstuff()$Hq_icon)
   my_map <- hideGroup(my_map, "COTA Stops")
   my_map <- hideGroup(my_map, "Satellite")
   my_map <- hideGroup(my_map, "OpenStreetMap")
   my_map <- addCircles(my_map, data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 1609.34, color = "green", stroke = T,
                 fillOpacity = .1, group = "1 Mile Radius",
                 popup = "One-mile radius circle, \n centered at reference.")
   my_map <- addCircles(my_map, data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 5*1609.34, color = "blue", stroke = T,
                 fillOpacity = .1, group = "5 Mile Radius",
                 popup = "Five-mile radius circle, \n centered at reference.")
   my_map <- addCircles(my_map, data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 10*1609.34, color = "yellow", stroke = T,
                 fillOpacity = .1, group = "10 Mile Radius",
                 popup = "Ten-mile radius circle, \n centered at reference.")
   my_map <- addCircles(my_map, data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 20*1609.34, color = "orange", stroke = T,
                 fillOpacity = .1, group = "20 Mile Radius",
                 popup = "Twenty-mile radius circle, \n centered at reference.")
   my_map <- addCircles(my_map, data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 30*1609.34, color = "red", stroke = T,
                 fillOpacity = .1, group = "30 Mile Radius",
                 popup = "Twenty-mile radius circle, \n centered at reference.")
   my_map <- addLayersControl(my_map, baseGroups = definedBaseGroups,
                       overlayGroups = definedOverlayGroups,
                       options = layersControlOptions(collapsed = FALSE))
   my_map <- hideGroup(my_map, "1 Mile Radius")
   my_map <- hideGroup(my_map, "5 Mile Radius")
   my_map <- hideGroup(my_map, "10 Mile Radius")
   my_map <- hideGroup(my_map, "20 Mile Radius")
   my_map <- hideGroup(my_map, "30 Mile Radius")    
  
      return(my_map)
  
}
usingPipes <- function(Coded, Mapstuff){
  
  definedOverlayGroups <- c("Reference", "Park & Ride", "COTA Stops", 
                            "1 Mile Radius", "5 Mile Radius", "10 Mile Radius", 
                            "20 Mile Radius", "30 Mile Radius")
  return(
    leaflet() %>%
      setView(lng = -82.9988, lat = 39.9612, zoom = 10) %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "ESRI") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addMeasure(position = "topleft", primaryLengthUnit = "miles",
                 primaryAreaUnit = "sqmiles") %>%
      {
        if("company" %in% colnames(Coded)){
          
          definedOverlayGroups <- c(definedOverlayGroups, unique(Coded$company))  
          
          Coded$companyFactor <- factor(Coded$company)
          factpal <- colorFactor(topo.colors(length(unique(Coded$company))), Coded$companyFactor)
          #for each unique company give it a different color
          #set label to be company
          addCircleMarkers(map=., data = Coded[-1, ], lat = ~lat, lng = ~lon, label = ~company,
                                         popup = ~company, color = ~factpal(companyFactor),
                                         group = ~company)
        }else{
          addCircleMarkers(map=., data = Coded[-1, ], lat = ~lat, lng = ~lon, label = ~address,
                                         popup = ~address, color = "green",
                                         clusterOptions = markerClusterOptions())
        }     
      }  %>%
      addMarkers(data = Mapstuff()$PR2, lng = ~lon, lat = ~lat,
                 label = ~name, group = "Park & Ride",
                 icon = Mapstuff()$Pnr_icon) %>%
      addMarkers(data = Mapstuff()$Stops, lng = ~lon, lat = ~lat,
                 label = ~stopname, group = "COTA Stops",
                 icon = Mapstuff()$Bus_icon) %>%
      addMarkers(data = Coded[1, ], lng = ~lon, lat = ~lat, label = ~address,
                 group = "Reference", icon = Mapstuff()$Hq_icon) %>%
      hideGroup("COTA Stops") %>%
      hideGroup("Satellite") %>%
      hideGroup("OpenStreetMap") %>% 
      addCircles(data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 1609.34, color = "green", stroke = T,
                 fillOpacity = .1, group = "1 Mile Radius",
                 popup = "One-mile radius circle, \n centered at reference.") %>%
      addCircles(data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 5*1609.34, color = "blue", stroke = T,
                 fillOpacity = .1, group = "5 Mile Radius",
                 popup = "Five-mile radius circle, \n centered at reference.") %>%
      addCircles(data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 10*1609.34, color = "yellow", stroke = T,
                 fillOpacity = .1, group = "10 Mile Radius",
                 popup = "Ten-mile radius circle, \n centered at reference.") %>%
      addCircles(data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 20*1609.34, color = "orange", stroke = T,
                 fillOpacity = .1, group = "20 Mile Radius",
                 popup = "Twenty-mile radius circle, \n centered at reference.") %>%
      addCircles(data = Coded[1, ], lng = ~lon, lat = ~lat,
                 radius = 30*1609.34, color = "red", stroke = T,
                 fillOpacity = .1, group = "30 Mile Radius",
                 popup = "Twenty-mile radius circle, \n centered at reference.") %>%
      addLayersControl(baseGroups = c("ESRI", "OpenStreetMap", "Satellite"),
                       overlayGroups = c("Reference", "Park & Ride", "COTA Stops", 
                                         "1 Mile Radius", "5 Mile Radius", "10 Mile Radius", 
                                         "20 Mile Radius", "30 Mile Radius"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("1 Mile Radius") %>% 
      hideGroup("5 Mile Radius") %>%
      hideGroup("10 Mile Radius") %>%
      hideGroup("20 Mile Radius") %>% 
      hideGroup("30 Mile Radius")    
  )
}
reusableLeaflet <- NULL

jitter_geocode <- function(lon, lat, km){
  length_at_equator <- 110.5742727 # in kilometers
  lon_degree_per_km <- 1 / (cos(lon * (2 * pi) / 360) * length_at_equator)
  jitter_lon <- jitter_coord(lon, lon_degree_per_km, km)
  lat_degree_per_km <- 1 / (length_at_equator)
  jitter_lat <- jitter_coord(lat, lat_degree_per_km, km)
  return (data.frame("lon"=jitter_lon, "lat"=jitter_lat))
}
jitter_coord <- function(coord, degree_per_km, km){
  jitter_coord <- coord + (runif(length(coord), min = -1, max = 1) * degree_per_km * km)
  return (jitter_coord)
}


mergedDataFrame <- NULL

mergeDataFrameFunction <- function(datapath, company){
  df <- read_csv(datapath)
  df$company <- company
  mergedDataFrame <<- rbind(mergedDataFrame, df)
}

server <- function(input, output, session) {
  
  observe({
    shinyjs::disable("downloadGeocodedData")  
    shinyjs::disable("downloadAnonymizedData")  
  })
  
  
  Mapstuff <- function(){
    Facilities <- rgdal::readOGR("Facilities_Jan18",
                                 layer = "Facilities_Jan2018",
                                 stringsAsFactors = F)
  
    Cota_stops <- rgdal::readOGR("May_2018", 
                                 layer = "Stops05_2018", 
                                 stringsAsFactors = F)
    Cota_stops@data$Lon <- as.double(Cota_stops@data$Lon)/1e6
    Cota_stops@data$Lat <- as.double(Cota_stops@data$Lat)/1e6
  
    Stops <- Cota_stops@data %>% 
      subset(is.na(tpField01), 
            select = c("StopName", "Lon", "Lat", "Shelter", "SignPost"))
  
    PR <- Cota_stops@data %>% 
      subset(is.na(tpField01) == F, 
            select = c("StopName", "Lon", "Lat", "Shelter", "SignPost"))
  
    PR2 <- Facilities@data %>% 
      subset(TYPE == "PR", 
            select = c("NAME", "Lon", "Lat", "SPACES", "Location"))
    
    colnames(Stops) <- tolower(colnames(Stops))
    colnames(PR) <- tolower(colnames(PR))
    colnames(PR2) <- tolower(colnames(PR2))
    
    bus <- makeIcon("Bus_logo.svg", 
                    iconWidth = 10, iconHeight = 10)
    pnr <- makeIcon("parking_p.png")
    hq <- makeIcon("map_point32.png")
    
    return(list(Stops = Stops, PR = PR, PR2 = PR2, Bus_icon = bus,
                Pnr_icon = pnr, Hq_icon = hq))
  }


  observeEvent(input$fileUpload, {
      
      #df <- read_csv("/Users/pgordon/ownCloud/Commuter Map/Geocoded_address.csv")
      df <- read_csv(input$fileUpload$datapath)
      if("lon" %in% names(df) && "lat" %in% names(df)){
        output$mapOutput <- renderMenu({
          menuItem("Map", icon = icon("th"), tabName = "map")   
        })
        
        output$mapDownloadOutput <- renderMenu({
          menuItem("Download Report", tabName = "download", icon = icon("dashboard"))      
        })         
        #use df that has been read and the lon lat values
        colnames(df) <- tolower(colnames(df))
        output$map <- renderLeaflet({
          
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = 'The map is rendering. This process may take several seconds.')
          
          reusableLeaflet <<- createLeaflet(df, Mapstuff)
          
        })
      } 
    
  })
  observeEvent(input$fileGeocode, {
    addresses <- read.table(input$fileGeocode$datapath, quote="", sep="\n", stringsAsFactors = F)
    address <- NULL
    mat <- data.frame(matrix(nrow = length(addresses), ncol = 3))
    colnames(mat) <- c("address", "lon", "lat")
    progress <- shiny::Progress$new(session, min=1, max=nrow(addresses))
    on.exit(progress$close())
    progress$set(message = 'The addresses are being geocoded and prepared for Download')
    start.time <- Sys.time()
    base <- "https://maps.googleapis.com/maps/api/"
    endpoint <- "geocode/json"
    key <- "<YOUR API KEY>"
    for(i in seq_len(nrow(addresses))){
      call1 <- paste(base,endpoint,"?","address","=", addresses[i,], "&key=", key, sep="")
      getResult <- GET(URLencode(call1))
      getResultText <- content(getResult, "text")
      result <- fromJSON(getResultText)
      mat[i,] <- c(result$results$formatted_address, result$results$geometry$location$lng, result$results$geometry$location$lat)
      progress$set(value=i)
    }
    
    shinyjs::enable("downloadGeocodedData")
    
    output$downloadGeocodedData <- downloadHandler(
      "geocoded_addresses.csv",
      content = function(file) {
        write.csv(mat, file, row.names = FALSE, quote=c(1))
      }
    )
    
  })
  observeEvent(input$fileAnonymize, {
    
    df <- read_csv(input$fileAnonymize$datapath)
    
    
    if("lon" %in% names(df) && "lat" %in% names(df)){
      
      anonymized_geocodes <- jitter_geocode(df[["lon"]], df[["lat"]], input$distance)
      anonymized_geocodes$address <- rep(input$namedGeocode, nrow(anonymized_geocodes))
      shinyjs::enable("downloadAnonymizedData")
      
      output$downloadAnonymizedData <- downloadHandler(
        "anonymized_geocodes.csv",
        content = function(file) {
          write.csv(anonymized_geocodes, file, row.names = FALSE, quote=c(1))
        }
      )
    
    }
  })
  
  observeEvent(input$fileMerge1, {
    mergedDataFrame <<- NULL
    mergeDataFrameFunction(input$fileMerge1$datapath, input$companyMerge1)
  })
  observeEvent(input$fileMerge2, {
    mergeDataFrameFunction(input$fileMerge2$datapath, input$companyMerge2)
  })  
  observeEvent(input$fileMerge3, {
    mergeDataFrameFunction(input$fileMerge3$datapath, input$companyMerge3)
  }) 
  observeEvent(input$fileMerge4, {
    mergeDataFrameFunction(input$fileMerge4$datapath, input$companyMerge4)
  })   
  output$downloadMergedData <- downloadHandler(
    "merged_geocodes.csv",
    content = function(file) {
      fileToWrite <<- mergedDataFrame
      print(fileToWrite)
      write.csv(fileToWrite, file, row.names = FALSE)
    }
  )   
  output$downloadReport <- downloadHandler(
    "map.html",
    content = function(file) {
      htmlwidgets::saveWidget(reusableLeaflet, file)
    }
  )
  
  
  
}

shinyApp(ui, server)
