# ### load functions
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_01_region_area.R')

library(terra)
library(sf)
library(tidyverse)
library(tidyrgee)
library(rgee)

rgee::ee_install_upgrade()

# Initialize just Earth Engine
ee_clean_user_credentials()
# ee_Authenticate()
ee_Initialize(drive = TRUE)
# Earth Engine account: users/ecordero 
# Python Path: /Users/EC13/.virtualenvs/rgee/bin/python 

# set path names
path.in <- '~/Documents/Projects/GIS/'
path.r <- paste0(path.in,'Raster/DEM/')
path.v <- paste0(path.in,'Vector/')

# crs
crs = 'EPSG:4326'

# evacuation area
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))
area.evac <- project(area.evac,crs)
area.study <- area.evac[area.evac$Municipio == 'San Juan',]
area.region <- region_area(area.evac,area.study)

# convert to ee aoi
aoi <- ee$Geometry$Rectangle(c(xmin(area.region),ymin(area.region),
                               xmax(area.region),ymax(area.region)))

# extract image data
images <- ee$ImageCollection('LANDSAT/LC09/C02/T1_L2') %>%
  ee$ImageCollection$filterBounds(aoi) %>%
  ee$ImageCollection$filterDate('2019-12-01', '2023-12-01') %>%
  ee$ImageCollection$filter(ee$Filter$lt('CLOUD_COVER',10)) %>%
  ee$ImageCollection$select('SR_B[1-7]') 

images <- tidyrgee::clip(images, aoi)
images <- tidyrgee::as_ee(images)

compos <- tidyrgee::as_tidyee(images)
compos <- ee_composite(compos, stat='median')
compos <- tidyrgee::as_ee(compos)
image <- images$median()

Map$centerObject(aoi,11)
Map$addLayer(images$first())


task_img <- ee_imagecollection_to_local(
  ic = images,
  region = aoi,
  dsn = paste0(path.in,'Raster/GEE/PR/')
)

# landsat files list
file.names <- list.files(path=paste0(path.in,'Raster/GEE/PR'),
                         pattern=".tif$")


# read in the training raster
# in this case, the raster is the first one within the list
# this raster has the lowest cloud cover (<1%)
s <- rast(paste0(path.in,'Raster/GEE/PR/',file.names[1]))

crs <- crs(s)
ext <- ext(project(area.region,crs))

r.list <- c()
for (i in 1:length(file.names)){
  # read in and prepare raster
  r <- rast(paste0(path.in,'Raster/GEE/PR/',file.names[i]))
  r <- terra::project(r, crs, method="bilinear")
  r <- resample(r,s, method="bilinear")
  ext(r) <- ext
  r.list <- c(r.list,r)
}

r <- sprc(r.list)
r <- mosaic(r, fun = 'median')

writeRaster(r,paste0(path.in,'Raster/GEE/PR/greater_sj.tif'))

plotRGB(r,r=4,g=3,b=2)
