### Libraries

# analysis
library(terra) 
library(fields)
library(raster)
library(sf)
library(movecost)
library(gdistance)
library(leastcostpath)

# Remove the temporary files to free up disk space
ff <- tmpFiles(old=TRUE)
tmpFiles(old=TRUE, remove=TRUE)

