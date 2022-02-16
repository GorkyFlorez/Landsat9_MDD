library(rgee)
library(googledrive)
library(stars)
library(geojsonio)
library(cptcity)
library(mapedit)
library(leaflet.extras2)
ee_Initialize("gflorezc", drive = T) 


Chonta <-ee$FeatureCollection("users/gflorezc/Cuen_Chonta")
ambito <- mapedit::drawFeatures()       # Creamos el objeto
ambito <- ambito %>% st_as_sf()         # Convertimos el objeto sf_ee

Chonta <- ee$Geometry$Rectangle(
  c(-69.25129 , -12.6357, -69.14555 ,-12.54955),
  geodesic = FALSE,
  proj = "EPSG:4326"
)

#2. Dataset --------------------------------------------------------------
  # Scaling factors
  scaling_img <- function(image) {
    opticalBands = image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
    thermalBands = image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
    image$addBands(opticalBands, NULL, TRUE)$
      addBands(thermalBands, NULL, TRUE)
  }

lista <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$
  filterDate("2022-01-01", "2022-01-16")$
  filterBounds(Chonta)

catalogo <- ee_get_date_ic(lista)                         # Catalogo de imagenes 
catalogo
# Landsat 9 scaled
l9 <- ee$Image('LANDSAT/LC09/C02/T1_L2/LC09_002069_20220105')$
  clip(Chonta) %>% 
  scaling_img()

NDVI      <- l9$normalizedDifference(c("SR_B5","SR_B4"))
# 3. Visualization of color false and ndvi --------------------------------
viz_ndvi <- list(min = -0.5,max = 0.5,palette = cpt("grass_ndvi"))
viz <- list(min = 0.07,max = 0.37,bands = c("SR_B5","SR_B4","SR_B3"))

Map$centerObject(Chonta)
m1 <-Map$addLayer(eeObject =NDVI , "NDVI Puerto Maldonado", visParams = viz_ndvi) +
  Map$addLegend(
    visParams = viz_ndvi)

m2 <- Map$addLayer(l9,visParams = viz)

m2 | m1 


sentinel2 <- ee$ImageCollection("COPERNICUS/S2")
Trueimage <-sentinel2$filterBounds(Chonta)$ 
  filterDate("2021-08-01", "2022-01-16")$ 
  sort("CLOUDY_PIXEL_PERCENTAGE", FALSE)$
  mosaic()$
  clip(Chonta)


trueColor <- list(min= 0,max= 3000, bands= c("B11","B8", "B2"))
NDVI_s    <- Trueimage$normalizedDifference(c("B8", "B4"))

Map$centerObject(Chonta)   
m3 <-Map$addLayer(Trueimage, visParams = trueColor,
             name= "Analisis de Agricultura")

m4 <-Map$addLayer(eeObject =NDVI_s , "NDVI_Sentinel", visParams = viz_ndvi) +
  Map$addLegend(
    visParams = viz_ndvi)


