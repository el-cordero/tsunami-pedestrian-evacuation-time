### Analysis - Run PEAT
### PEAT created by Elvin Cordero in R

# requires:

# for macOS
# ### load libraries
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/01_libraries.R')
# 
# ### load functions
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_01_region_area.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_02_escape_points.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_03_minimum_distance.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_04_distance_grid.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_05_pea.R')

# path inputs
path.in <- '~/Documents/Projects/GIS/'
path.r <- paste0(path.in,'Raster/dem2/')
path.v <- paste0(path.in,'Vector/')

# for windows
# ### load libraries
# source('~/Projects/Data/R/Pedestrian Analysis/01_libraries.R')
#
# ### load functions
# source('~/Projects/Data/R/Pedestrian Analysis/02_func_01_region_area.R')
# source('~/Projects/Data/R/Pedestrian Analysis/02_func_02_escape_points.R')
# source('~/Projects/Data/R/Pedestrian Analysis/02_func_03_minimum_distance.R')
# source('~/Projects/Data/R/Pedestrian Analysis/02_func_04_distance_grid.R')
# source('~/Projects/Data/R/Pedestrian Analysis/02_func_05_pea.R')

# # path inputs
# path.in <- '~/Projects/Data/GIS/'
# path.r <- paste0(path.in,'Raster/Pedestrian/')
# path.v <- paste0(path.in,'Vector/base/')

# coordinate reference system
crs <- 'epsg:3920'

# evacuation area
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))
area.evac <- project(area.evac,crs)

# roads
rds <- vect(paste0(path.v,'road_pr.shp'))
rds <- project(rds,crs)
rds <- rds[!(rds$highway %in% c('track','path','service',
                                'footway','cycleway')),]

# evacuation grid polygon
grid.evac <- vect(paste0(path.v,'poly_grid.shp'))

# create a file list of dems
dem.list <- c('dem_aguada.tif','dem_aguadilla.tif','dem_anasco.tif','dem_arecibo.tif',
'dem_arroyo.tif','dem_barc.tif','dem_bayamon.tif','dem_caborojo.tif',
'dem_camuy.tif','dem_canovanas.tif','dem_carolina.tif','dem_catano.tif',
'dem_ceiba.tif','dem_culebra.tif','dem_dorado.tif','dem_fajardo.tif',
'dem_guanica.tif','dem_guayama.tif','dem_guayan.tif','dem_guaynabo.tif',
'dem_hatillo.tif','dem_humacao.tif','dem_isabela.tif','dem_juana.tif',
'dem_lajas.tif','dem_loiza.tif','dem_luquillo.tif','dem_manati.tif',
'dem_maunabo.tif','dem_mayaguez.tif','dem_naguabo.tif','dem_patillas.tif',
'dem_penuelas.tif','dem_ponce.tif','dem_quebradillas.tif','dem_rincon.tif',
'dem_riogrande.tif','dem_salinas.tif','dem_sanjuan.tif','dem_santaisabel.tif',
'dem_toabaja.tif','dem_vegaalta.tif','dem_vegabaja.tif','dem_vieques.tif',
'dem_yabucoa.tif', 'dem_yauco.tif')
missing.dems <- c('dem_canovanas.tif','dem_ceiba.tif','dem_humacao.tif',
                  'dem_quebradillas.tif','dem_vieques.tif','dem_yauco.tif')
dem.list <- dem.list[!dem.list %in% missing.dems]

municipalities <- sort(unique(area.evac$Municipio))
fraction <- 1:length(municipalities) 

for (i in 1:length(municipalities)){
  if (municipalities[i] == c('Arecibo')){
    fraction[i] <- 400
  } else (fraction[i] <- 150)
}

fraction <- 1500

missing.muni <- c("Canóvanas","Ceiba","Humacao","Quebradillas","Vieques","Yauco")
municipalities <- municipalities[!municipalities %in% missing.muni]

# Run on the pa grid function on each municipality
for (i in c(1:length(dem.list))){
# for (i in c(13,37)){
  startTime <- Sys.time()
  dem <- rast(paste0(path.r,dem.list[i]))
  pa.grid <- pa_grid(area.evac, dem, rds, grid.evac, 
                     municipalities[i],fraction, crs)
  file.name <- paste0(path.v,paste0('Pedestrian/',paste0(municipalities[i]),'.shp'))
  writeVector(pa.grid,file.name,overwrite=TRUE)
  endTime <- Sys.time()
  cat(paste(municipalities[i],startTime,endTime,sep=','), 
      file = paste0(municipalities[i],".txt"))
}

# # Run on the pa grid function on each municipality
# for (i in c(3:length(missing.dems))){
#   print(paste0(missing.muni[i],' processing started'))
#   print(Sys.time())
#   dem <- rast(paste0(path.r,missing.dems[i]))
#   pa.grid <- pa_grid(area.evac, dem, rds, grid.evac, missing.muni[i], crs)
#   file.name <- paste0(path.v,paste0('Pedestrian/',paste0(missing.muni[i]),'.shp'))
#   writeVector(pa.grid,file.name,overwrite=TRUE)
#   print(paste0(missing.muni[i],' was finished processing'))
#   print(Sys.time())
# }

# combine files
all.names = list.files(path = paste0(path.v,'Pedestrian/'),full.names=FALSE)
all.names <- unlist(strsplit(all.names,'[.]'))
filetypes <- c("prj","cpg","shp","dbf","shx")
all.names <- unique(all.names[!all.names %in% filetypes])
all.names <- paste0(all.names,'.shp')

# open the first file
pa.grid <- vect(paste0(path.v,paste0('Pedestrian/',all.names[1])))

# combine with the other files
for (i in c(2:length(all.names))){
  pa.grid.2 <- vect(paste0(path.v,paste0('Pedestrian/',all.names[i])))
  pa.grid <- rbind(pa.grid,pa.grid.2)
}

# save the file into one
writeVector(pa.grid,paste0(path.v,'PRpeat.shp'),overwrite=TRUE)

tmpFiles(old=TRUE, remove=TRUE)
