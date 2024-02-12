
# split ceiba into divisions 
ceiba <- vect(paste0(path.v,'ceiba_divisions.shp'))
ceiba$id <- 1:length(ceiba)

# add Municipio field
ceiba$Municipio <- paste0('Ceiba_',ceiba$id)

# make a template vector using evacuation area
ceiba.mask <- area.evac[1]
ceiba.mask <- ceiba.mask[-1]

# crop evacuation area by cabo rojo divisions
for (i in 1:length(ceiba)){
  cr <- crop(area.evac[area.evac$Municipio=='Ceiba',],
             ceiba[i])
  cr$Municipio <- ceiba[i]$Municipio
  cr$Municipio2 <- 'Ceiba'
  ceiba.mask <- rbind(ceiba.mask,cr)
}

# add new field to differentiate the new Cabo Rojo divisions
area.evac$Municipio2 <- area.evac$Municipio

# remove cabo rojo
area.evac <- area.evac[area.evac$Municipio!='Ceiba',]

# add in cabo rojo
area.evac <- rbind(area.evac,ceiba.mask)


# Run on the pa grid function on Cabo Rojo

dem <- rast(paste0(path.r,'dem_ceiba.tif'))
for (i in 1:length(ceiba$Municipio)){
  # for (i in c(13,37)){
  startTime <- Sys.time()
  pa.grid <- pa_grid(area.evac, dem, rds, grid.evac,
                     ceiba$Municipio[i],fraction, crs)
  file.name <- paste0(path.v,'Pedestrian/Ceiba/',ceiba$Municipio[i],'.shp')
  writeVector(pa.grid,file.name,overwrite=TRUE)
  endTime <- Sys.time()
  outputMsg <- paste(ceiba$Municipio[i],startTime,endTime,sep=',')
  print(outputMsg)
  cat(outputMsg, file = paste0('Timestamps/',ceiba$Municipio[i],".txt"))
}

# run the analysis for Cabo Rojo only
ceiba.grid <- vect(paste0(path.v,'Pedestrian/',ceiba$Municipio[1],'.shp'))
for (i in 2:length(ceiba$Municipio)){
  ceiba.grid.2 <- vect(paste0(path.v,'Pedestrian/',ceiba$Municipio[i],'.shp'))
  ceiba.grid <- rbind(ceiba.grid,ceiba.grid.2)
}
file.name <- paste0(path.v,'Pedestrian/','Ceiba','.shp')
writeVector(ceiba.grid,file.name,overwrite=TRUE)

