# data validation

source('_scripts/00_libraries.R')

# path inputs
path.in <- '~/Documents/Projects/PRSN/Data/GIS/'
path.r <- paste0(path.in,'Raster/dem2/')
path.v <- paste0(path.in,'Vector/')

crs <- 'epsg:4326' #wgs 84

subdivs <- vect("~/Documents/Projects/PRSN/Data/GIS/Vector/PR/PR Census/tl_2016_72_cousub")
subdivs <- terra::project(subdivs, crs)

names <- c('Barrero', 'Calvache', 'Ensenada', 'Pueblo', 'Puntas', 'Rincón', 'Río Grande')
subdivs <- subdivs[subdivs$NAME %in% names]
subdivs[subdivs$NAME == 'Rincón']$NAME <- 'Rincón Pueblo'
subdivs[subdivs$NAME == 'Pueblo']$NAME <- 'Pueblo (Córcega)'

# evacuation area
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))
area.evac <- terra::project(area.evac, crs)
area.study <- area.evac[area.evac$Municipio == 'Rincón']

subdivs <- crop(subdivs,area.study)

evac.data <- vect(paste0(path.v,'PRpeatWGS84_pnts.shp'))
evac.data <- terra::project(evac.data, crs)
evac.data <- evac.data[evac.data$Municipio == 'Rincón']
evac.data$slow_walk <- evac.data$distance * (1/1.1) * (1/60) 

prsn <- vect(paste0(path.v,'Pedestrian/old/v_a_rincon.shp'))
prsn <- terra::project(prsn,crs)
prsn$dist <- prsn$gridcode * (1.22) * (60)
prsn$prsn <- prsn$dist * (1/1.1) * (1/60) 

evac.data <- intersect(evac.data, subdivs['NAME'])
evac.data <- intersect(evac.data, prsn['prsn'])

evac.val <- evac.data %>% 
  group_by(NAME) %>%
  summarise(mean = round(mean(slow_walk),2),
            # median = median(slow_walk),
            # max = max(slow_walk)
            prsn = round(mean(prsn),2)) 

evac.val$Faucher_ATT <- c(10.94, 26.26, 12.53, 24.24, 6.42, 7.920)

evac.val %>% data.frame()

writeVector(evac.data[c('NAME','slow_walk','prsn')],paste0(path.v,'validation.shp'),overwrite=TRUE)

write.csv(evac.val,'_documentation/_written/Markdown/_tables/validation.csv',
          row.names = FALSE)

