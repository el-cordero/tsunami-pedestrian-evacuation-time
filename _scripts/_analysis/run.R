### Analysis - Run PEAT
### PEAT created by Elvin Cordero in R

## requires:
# for macOS
# ### load libraries
source('_scripts/00_libraries.R')
# 
# ### load functions
source('_scripts/_functions/01_region_area.R')
source('_scripts/_functions/02_escape_points.R')
source('_scripts/_functions/03_calc_lc_path.R')
source('_scripts/_functions/03_calc_min_distance.R')
source('_scripts/_functions/03_minimum_distance.R')
source('_scripts/_functions/04_distance_grid.R')
source('_scripts/_functions/05_pea.R')

# path inputs
path.in <- '~/Documents/Projects/PRSN/Data/GIS/'
path.r <- paste0(path.in,'Raster/dem2/')
path.v <- paste0(path.in,'Vector/')

# coordinate reference system
crs <- 'epsg:3920'
crs2 <- 'epsg:4326' #wgs 84

# evacuation area
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))

grid.evac <- vect(paste0(path.v,'poly_grid.shp'))

# split cabo rojo into divisions 
cabo.rojo <- vect(paste0(path.v,'caborojo_divisions.shp'))
cabo.rojo <- terra::project(cabo.rojo,area.evac)
plot(cabo.rojo)
plot(area.evac,add=TRUE,col='blue')
# add Municipio field
cabo.rojo$Municipio <- paste0('Cabo Rojo_',cabo.rojo$id)

# make a template vector using evacuation area
cabo.rojo.mask <- area.evac[1]
cabo.rojo.mask <- cabo.rojo.mask[-1]

# crop evacuation area by cabo rojo divisions
for (i in 1:length(cabo.rojo)){
  cr <- crop(area.evac[area.evac$Municipio=='Cabo Rojo',],
             cabo.rojo[i])
  cr$Municipio <- cabo.rojo[i]$Municipio
  cr$Municipio2 <- 'Cabo Rojo'
  cabo.rojo.mask <- rbind(cabo.rojo.mask,cr)
}

# add new field to differentiate the new Cabo Rojo divisions
area.evac$Municipio2 <- area.evac$Municipio

# remove cabo rojo
area.evac <- area.evac[area.evac$Municipio!='Cabo Rojo',]

# add in cabo rojo
area.evac <- rbind(area.evac,cabo.rojo.mask)

# roads
rds <- vect(paste0(path.v,'road_pr.shp'))
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

# these are dems that will not be used or will
# be used individually
missing.dems <- c('dem_caborojo.tif','dem_canovanas.tif','dem_ceiba.tif','dem_humacao.tif',
                  'dem_quebradillas.tif','dem_vieques.tif','dem_yauco.tif')
dem.list <- dem.list[!dem.list %in% missing.dems]

# make a list of the municipalities
# remove the municipalities removed from the raster list
missing.muni <- c("Cabo Rojo","Canóvanas","Ceiba","Humacao","Quebradillas","Vieques","Yauco")
municipalities <- sort(unique(area.evac$Municipio))
municipalities <- municipalities[!municipalities %in% missing.muni]

# municipality-specific evac zone
area.evac <- project(area.evac,crs(rds))

# produce the escape pnts
# escape.pnts <- escape_points(area.evac,rds)
# writeVector(escape.pnts, '~/Documents/Projects/PRSN/Data/GIS/Vector/escape_pnts_PR.shp')
escape.pnts <- vect('~/Documents/Projects/PRSN/Data/GIS/Vector/escape_pnts_PR_edited.shp')

# What fraction of road points to use?
# fraction <- 500 # 1000 was used for all municipalities except
# fraction <- 700 # cabo rojo
# fraction <- 1500 # arecibo?

# Run on the pa grid function on each municipality
for (i in 1:length(dem.list)){
# for (i in 2:length(dem.list)){
  startTime <- Sys.time()
  dem <- rast(paste0(path.r,dem.list[i]))
  pa.grid <- pa_grid(area.evac, dem, rds, escape.pnts, 
                     municipalities[i],fraction=NULL, crs, rs=5)
  file.name <- paste0(path.v,'Pedestrian/',municipalities[i]) 
  writeVector(pa.grid[1],paste0(file.name,'_pnts.shp'),overwrite=TRUE)
  writeVector(pa.grid[2],paste0(file.name,'.shp'),overwrite=TRUE)
  endTime <- Sys.time()
  outputMsg <- paste(municipalities[i],startTime,endTime,
                     length(pa.grid[1]),sep=',')
  print(outputMsg)
  cat(outputMsg, file = paste0(municipalities[i],".txt"))
}

# Run on the pa grid function on Cabo Rojo

dem <- rast(paste0(path.r,'dem_caborojo.tif'))
for (i in 1:length(cabo.rojo$Municipio)){
  startTime <- Sys.time()
  dem <- rast(paste0(path.r,dem.list[i]))
  pa.grid <- pa_grid(area.evac, dem, rds, escape.pnts, 
                     cabo.rojo$Municipio[i],fraction=NULL, crs, rs=5)
  file.name <- paste0(path.v,'Pedestrian/CR/',cabo.rojo$Municipio[i]) 
  writeVector(pa.grid[1],paste0(file.name,'_pnts.shp'),overwrite=TRUE)
  writeVector(pa.grid[2],paste0(file.name,'.shp'),overwrite=TRUE)
  endTime <- Sys.time()
  outputMsg <- paste(cabo.rojo$Municipio[i],startTime,endTime,
                     length(pa.grid[1]),sep=',')
  print(outputMsg)
  cat(outputMsg, file = paste0(cabo.rojo$Municipio[i],".txt"))
}

# for (i in c(13,37)){
# startTime <- Sys.time()
# pa.grid <- pa_grid(area.evac, dem, rds, grid.evac,
#                    cabo.rojo$Municipio[i],fraction, crs)
# file.name <- paste0(path.v,'Pedestrian/CR/',cabo.rojo$Municipio[i],'.shp')
# writeVector(pa.grid,file.name,overwrite=TRUE)
# endTime <- Sys.time()
# outputMsg <- paste(cabo.rojo$Municipio[i],startTime,endTime,sep=',')
# print(outputMsg)
# cat(outputMsg, file = paste0('Timestamps/',cabo.rojo$Municipio[i],".txt"))

# run the analysis for Cabo Rojo only
cr.grid <- vect(paste0(path.v,'Pedestrian/',cabo.rojo$Municipio[1],'.shp'))
for (i in 2:length(cabo.rojo$Municipio)){
  cr.grid.2 <- vect(paste0(path.v,'Pedestrian/',cabo.rojo$Municipio[i],'.shp'))
  cr.grid <- rbind(cr.grid,cr.grid.2)
}

file.name <- paste0(path.v,'Pedestrian/','Cabo Rojo','.shp')
writeVector(cr.grid,file.name,overwrite=TRUE)

# combine files
all.names = list.files(path = paste0(path.v,'Pedestrian/'),full.names=FALSE)
all.names <- unlist(strsplit(all.names,'[.]'))
filetypes <- c("prj","cpg","shp","dbf","shx")
all.names <- unique(all.names[!all.names %in% filetypes])
all.names <- paste0(all.names,'.shp')
all.names <- all.names[all.names != "CR.shp"] # remove CR folder

# open the first file
pa.grid <- vect(paste0(path.v,paste0('Pedestrian/',all.names[1])))

# combine with the other files
for (i in c(2:length(all.names))){
  pa.grid.2 <- vect(paste0(path.v,paste0('Pedestrian/',all.names[i])))
  pa.grid <- rbind(pa.grid,pa.grid.2)
}

# ? remove 0 Municipality layer?
pa.grid <- pa.grid[pa.grid$Municipio != "0",]

# save the file into one
writeVector(pa.grid,paste0(path.v,'PRpeat.shp'),overwrite=TRUE)

pa.grid <- terra::project(pa.grid,crs2)
writeVector(pa.grid,paste0(path.v,'PRpeatWGS84.shp'),overwrite=TRUE)
