##global R
##version 0.52 ## Improve CSS thanks to CER!
  ## Coordinate display center
  ## Zoom button moved down
  ## Sidebar Colors Changed
  ## Transparency removed on leaflet tools for consistency
  ## Background color added to floating panel
  ## Images rounded
  ## Spacing improved on legend
  ## Fonts and colors modified
##version 0.50 ## Update 'recommended' data layers
  ## Add mapview dependency
  ## Add MouseCoordinates2() : 
  ## Remove click for coordinates
##version 0.44 ## Not deployed
##version 0.43 ## Replace 'proposed' with 'recommended' throughout
##version 0.42 ## Replace coral data source with cleaned spatial data
##version 0.40 ## Replace coral data source with cleaned spatial data
##version 0.34 ## Replace coral data source with cleaned spatial data
##version 0.33 ## see change log
##version 0.32 ## Added about page
##version 0.31 ## Added cleaned coral points
##version 0.1.1
#setwd("X:/Data_John/shiny/coralhapcv052")
#setwd("X:/Data_John/shiny/coralhapcv042")
#setwd("X:/Data_John/shiny/coralhapcv040")
#setwd("C:/Users/Froeschke/John/GMFMC/shiny/coralhapcv034")
#setwd("X:/Data_John/shiny/coralhapcv050")
##use development version of leaflet to leverage new features 11.3.2015
library(devtools)
if (!require('leaflet')) devtools::install_github('rstudio/leaflet@v1.0.1.9004')

library(shinydashboard)
library(leaflet)
library(rgdal)
library(DT)
library(mapview)

load("HAPCs.RData")
load("HAPCwRegsv010.RData")
load("cpa.RData")
load("depth50m.RData")
#head(HAPCwRegs@data)

#load("coralpoints.RData")
#load("coralpoints5.RData")

load("coralpointsv023.RData")

### split layers for layer control
LayerNames <- unique(coralpoints2@data$Name)
StonyCoral <- subset(coralpoints2, coralpoints2$Name=="Stony coral")
BlackCoral <- subset(coralpoints2, coralpoints2$Name=="Black coral")
SoftCoral <- subset(coralpoints2, coralpoints2$Name=="Soft coral")
Seapen <- subset(coralpoints2, coralpoints2$Name=="Sea pen")
Octocoral <- subset(coralpoints2, coralpoints2$Name=="Octocoral")
Sponge <- subset(coralpoints2, coralpoints2$Name=="Sponge")

##clean-up pHAPC
pHAPC$AREA_GEO <- round(pHAPC$AREA_GEO,1) ##better table display v0.33

## get units in nm^2 for HAPCwRegs, Thanks Bryan Schoonard for these numbers

x <- HAPCwRegs@data
x[1,2] <- 38.10  ##FGB East 38.10
x[2,2] <- 47.36  ##FGB West 47.36
x[3,2] <- 18.67  ##McGrail Bank 18.67
x[4,2] <- 449.30  ##Middle Grounds 449.30  Sq Miles
x[5,2] <- 133.27  ##Pulley Ridge 133.27
x[6,2] <- 2.33  ##Stetson Bank 2.33
x[7,2] <- 72.28  ##Tortugas South 72.28
x[8,2] <- 15.98  ##Tortugas North 15.98
x[9,2] <- 516.49  ##Edges
x[10,2] <- 141.27  ##Steamboat Lumps
x[11,2] <- 152.61  ##Madison Swanson
#x[11,4] <- "Madison Swanson"
rownames(x) <- NULL
HAPCwRegs@data <- x
rm(x)

##extract dataframe and get centroids of polygonsh
cpadf <- cpa@data
coords <- coordinates(cpa)
colnames(coords) <- c("x","y")
cpadfcoords <- cbind(cpadf, coords)

FishingRegulations <- data.frame(id=0:22,Name=cpadfcoords$Name,
                                 regs=c("Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "No",
                                    "No",
                                    "No",
                                    "No",
                                    "No",
                                    "Yes",
                                    "No",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes",
                                    "Yes"))
                                    


## extract dataframe and get centroids of polygons
pHAPC@data$AREA_GEO <- pHAPC@data$AREA_GEO * 0.386102 ##new version 0.40
pHAPCdf <- pHAPC@data
coords <- coordinates(pHAPC)
colnames(coords) <- c("x","y")
pHAPCdfcoords <- cbind(pHAPCdf, coords)



HAPCwregsdf <- HAPCwRegs@data
coords2 <- coordinates(HAPCwRegs)
colnames(coords2) <- c("x","y")
HAPCwregsdfcoords <- cbind(HAPCwregsdf, coords2)

HAPCnoregsdf <- HAPCnoregs@data
coords3 <- coordinates(HAPCnoregs)
colnames(coords3) <- c("x","y")
HAPCnoregsdfcoords <- cbind(HAPCnoregsdf, coords3)

HAPCneedrevisiondf <- HAPCneedrevision@data
coords4 <- coordinates(HAPCneedrevision)
colnames(coords4) <- c("x","y")
HAPCneedrevisioncoords <- cbind(HAPCneedrevisiondf, coords4)


##Calculate total area
#Area <- sum(HAPCwregsdfcoords$st_area_sh) + (sum(pHAPCdfcoords$AREA_GEO) * 0.386102) ##removed conversion v0.40
Area <- sum(HAPCwregsdfcoords$st_area_sh) + (sum(cpadf$Area))
#Area2 <- data.frame(Total=c(sum(HAPCwregsdfcoords$st_area_sh), (sum(pHAPCdfcoords$AREA_GEO) * 0.386102)))

##legend for coral locations
# pal <- colorFactor(c("#D0FA58", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
#                    domain = c("Black coral", "Hydrozoan", "Octocoral",
#                               "Sea anemones", "Stony Coral"))

#depth50m <- readOGR("50MeterBathy.shp", layer="50MeterBathy")


##Data Request
#writeOGR(HAPCwRegs, "K:/hanson/HAPCwRegs.shp", layer="HAPCwRegs", driver="ESRI Shapefile")
#writeOGR(pHAPC, "K:/hanson/pHAPC.shp", layer="pHAPC", driver="ESRI Shapefile")
#writeOGR(HAPCnoregs, "K:/hanson/HAPCnoregs.shp", layer="HAPCnoregs", driver="ESRI Shapefile")

####========================== Added version 0.50
# addMouseCoordinates2 <- function (map) 
# {
#   if (inherits(map, "mapview")) 
#     map <- mapview2leaflet(map)
#   stopifnot(inherits(map, "leaflet"))
#   map <- htmlwidgets::onRender(map, "\nfunction(el, x, data) {\n  // we need a new div element because we have to handle\n  // the mouseover output seperately\n  debugger;\n  function addElement () {\n    // generate new div Element\n    var newDiv = $(document.createElement('div'));\n    // append at end of leaflet htmlwidget container\n    $(el).append(newDiv);\n    //provide ID and style\n    newDiv.addClass('lnlt');\n    newDiv.css({\n      'position': 'relative',\n      'bottomleft':  '0px',\n      'background-color': 'rgba(255, 255, 255, 1)',\n      'box-shadow': '0 0 2px #bbb',\n      'background-clip': 'padding-box',\n      'margin': '0',\n      'color': '#333',\n      'font': '12px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',\n    });\n    return newDiv;\n  }\n\n  // check for already existing lnlt class to not duplicate\n  var lnlt = $(el).find('.lnlt');\n  if(!lnlt.length) {\n    lnlt = addElement();\n    // get the leaflet map\n    var map = HTMLWidgets.find('#' + el.id);\n\n    // grab the special div we generated in the beginning\n    // and put the mousmove output there\n    map.on('mousemove', function (e) {\n      lnlt.text('Latitude: ' + (e.latlng.lat).toFixed(5) +\n      ' | Longitude: ' + (e.latlng.lng).toFixed(5) +\n      ' | Zoom: ' + map.getZoom() + ' '\n      );\n    })\n  };\n}\n")
#   map
# }

cpaVK906 <- subset(cpa, cpa$Name=="Viosca Knoll 862/906")
#plot(cpaVK906)
cpaVK826 <- subset(cpa, cpa$Name=="Viosca Knoll 826")
#plot(cpaVK826, axes=TRUE)
## Experimental
 # bathy <- raster("bathy.tif")

