### load functions
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/_figures/figure_libraries.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/01_libraries.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_01_region_area.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_02_escape_points.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_03_minimum_distance.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_04_distance_grid.R')

# path inputs
path.in <- '~/Documents/Projects/GIS/'
path.r <- paste0(path.in,'Raster/DEM/')
path.v <- paste0(path.in,'Vector/')

crs = 'EPSG:4326'

pr <- vect(paste0(path.v,'PR/Municips/PR_edit.shp'))
pr <- project(pr,crs)
pr <- aggregate(pr)

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
grid.evac <- project(grid.evac,crs)

# municipality-specific evac zone
area.study <- area.evac[area.evac$Municipio == 'San Juan',]

#calculate region area
area.region <- region_area(area.evac,area.study)

minDist.pnts <- vect(paste0(path.v,'minDist_sj.shp'))
area.region <- project(area.region,minDist.pnts)
area.study <- project(area.study,minDist.pnts)

dist.grid <- distance_grid(minDist.pnts,area.region,
                           area.study,res.1 = 10,res.2 = 1)
r <- rast(ext = ext(dist.grid),crs = crs(dist.grid), resolution=100)
dist.grid <- resample(dist.grid,r)
writeRaster(dist.grid, paste0(path.in,'Raster/Pedestrian/distance_grid.tif'),
            overwrite=TRUE)
dist.grid <- rast(paste0(path.in,'Raster/Pedestrian/distance_grid.tif'))

time.grid <- dist.grid * (1/1.22) * (1/60) 

area.study <- project(area.study,crs)

# tile <- get_tiles(ext(area.study), provider = "OpenStreetMap", zoom = 13, cachedir = ".")
# writeRaster(tile, paste0(path.in,'Raster/OpenStreetMap/PR/sj_zoom12.tif'), overwrite=TRUE)
tile <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/sj_zoom12.tif'))

gg.time <- ggplot() +
  geom_spatraster_rgb(data = tile, maxcell=Inf) +   
  geom_spatvector(data = area.region, fill = 'yellow', alpha=0.3) +
  geom_spatraster(data = time.grid) +
  scale_fill_whitebox_c(palette = "bl_yl_rd") +
  coord_sf(crs = 4326,xlim =c(-66.13,-66.01),ylim=c(18.41,18.47526),
           clip = 'on', expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.79,0.85), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=4),
        legend.text = element_text(size=4),legend.background = element_blank()) +
  labs(fill='Evacuation Time (min)') +
  annotation_scale(location = "tr", width_hint = 0.25)

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/time2.png'
png(filename=filename, units="in", width=5.2, height=3.4, res=400)
gg.time
invisible(dev.off())

tile2 <- get_tiles(ext(c(-66.0975,-66.0775,18.4555,18.4685)), provider = 'Esri.WorldGrayCanvas', 
                   zoom = 16, cachedir = ".")
writeRaster(tile2, paste0(path.in,'Raster/OpenStreetMap/PR/osj_zoom16_esri_gray.tif'),
            overwrite=TRUE)
tile2 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/osj_zoom16_esri_gray.tif'))

time.rds <- vect('~/Documents/Projects/GIS/Vector/Pedestrian/San Juan.shp')
time.rds <- project(time.rds,time.grid)
time.grid <- mask(time.grid,time.rds)
time.grid <- project(time.grid,crs)
time.grid.crop <- crop(time.grid,ext(c(-66.0975,-66.0775,18.4554,18.4685)))


gg.roads <- ggplot() +
  geom_spatraster_rgb(data = tile1, maxcell=Inf) +
  geom_spatvector(data = area.region, fill = 'yellow', 
                  alpha=0.1) +
  geom_spatraster(data = time.grid.crop, maxcell = Inf) +
  scale_fill_whitebox_c(palette = "bl_yl_rd") +
  coord_sf(crs = 4326, xlim = c(-66.0975,-66.0775),ylim = c(18.4554,18.4685),
           clip = 'on', expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.79,0.85), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=4),
        legend.text = element_text(size=4),legend.background = element_blank()) +
  labs(fill='Evacuation Time (min)') +
  annotation_scale(location = "tr", text_cex = 0.7, width_hint = 0.10)

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/time_roads.png'
png(filename=filename, units="in", width=5.2, height=3.7, res=400)
gg.roads
invisible(dev.off())
