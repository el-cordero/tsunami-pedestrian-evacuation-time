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

# produce the escape pnts
# escape.pnts <- escape_points(area.evac,area.region,rds)
escape.pnts <- vect(paste0(path.v,'escape_pnts_sj.shp'))

area.region <- project(area.region,crs)

dem.rds <- rast(paste0(path.in,'Raster/dem2/dem_sanjuan.tif'))

e <- vect(paste0(path.v,'extent_osj.shp'))
e <- project(e,crs(dem.rds))

dem.rds.crop <- crop(dem.rds,e)

# escape.pnts <- project(escape.pnts,crs(dem))
# escape.pnts.crop <- crop(escape.pnts,e)
# set.seed(5)
# escape.pnts.crop <- sample(escape.pnts.crop,5)
# writeVector(escape.pnts.crop,paste0(path.v,'escape_pnts_sj_sample.shp'),overwrite=TRUE)
escape.pnts.crop <- vect(paste0(path.v,'escape_pnts_sj_sample.shp'))

# # roads
rds <- project(rds,crs(dem.rds.crop))
rds.crop <- crop(rds,e,ext=TRUE)
rds.pnts <- as.points(rds.crop) 
rds.pnts <- crds(rds.pnts,df=TRUE)
rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem.rds.crop))
rds.pnts.1 <- rds.pnts[1]

cs <- create_slope_cs(dem.rds.crop)

lc.paths <- create_lcp(cs,origin=rds.pnts.1,escape.pnts.crop)
lc.paths <- project(lc.paths,crs(dem.rds.crop))
lc.paths <- crop(lc.paths,e)

escape.pnts.crop$Distance <- round(c(perim(lc.paths),NA))

escape.pnts.crop$`Point Type` <- 'Safety Point'
rds.pnts.1$`Point Type` <- 'Road Point'
pnts <- rbind(escape.pnts.crop,rds.pnts.1)
pnts$`Point Type` <- as.factor(pnts$Point.Type)




# tile2 <- get_tiles(ext(c(-66.13,-66.01,18.3985,18.47526)), provider = "OpenStreetMap", zoom = 13, cachedir = ".")
# plot(tile2)
# writeRaster(tile2, paste0(path.in,'Raster/OpenStreetMap/PR/sj_zoom12.tif'), overwrite=TRUE)
tile2 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/sj_zoom12.tif'))


gg.region <- ggplot() + 
  geom_spatraster_rgb(data = tile2, maxcell=Inf) + 
  # geom_spatvector(data = pr, fill = "lightgreen", alpha=0.8,show.legend = FALSE) +
  geom_spatvector(data = area.region, fill = 'yellow', alpha=0.3, 
                  aes(shape='Evacuation Zone')) +
  coord_sf(crs = 4326, xlim =c(-66.13,-66.01),ylim=c(18.41,18.47526),
           clip = 'on', expand = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),legend.position = c(0.89,0.64),
        legend.text = element_text(size=5),axis.text=element_text(size=4),
        legend.background = element_blank()) +
  annotation_scale(location = "tr", width_hint = 0.25)
gg.region

bb <- vect(ext(area.region), crs=area.region)
# tile3 <- get_tiles(ext(area.evac[!(area.evac$Municipio %in% c("Culebra","Vieques")),]), 
                   # provider = "OpenStreetMap", zoom = 9, cachedir = ".")
# writeRaster(tile3, paste0(path.in,'Raster/OpenStreetMap/PR/pr_zoom9.tif'))
tile3 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/pr_zoom9.tif'))

ggm2 = ggplot() + 
  geom_spatraster_rgb(data = tile3, maxcel=Inf) +
  # geom_spatvector(data = aggregate(
  #   area.evac[!(area.evac$Municipio %in% c("Culebra","Vieques")),]), 
  #   aes(fill = Municipio), fill = 'yellow', show.legend = FALSE) +
  geom_spatvector(data = bb, fill = NA, 
                color = "red", lwd = 1) +
  coord_sf(crs = 4326, xlim=c(-67.3014360663465, -65.5473390968811),
           ylim=c(17.8811977500556, 18.5259765749779),
           clip = 'on', expand = FALSE) + 
  theme_void()  +
  theme(panel.border = element_rect(color = 'black',fill = NA,size = 0.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggm2

ggm = ggdraw() +
  draw_plot(gg.region) +
  draw_plot(ggm2, x = 0.68, y = 0.61, width = 0.3, height = 0.3)
ggm
filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/StudyRegion_SJ2.png'
png(filename=filename, units="in", width=5.2, height=3.4, res=400)
ggm
invisible(dev.off())

