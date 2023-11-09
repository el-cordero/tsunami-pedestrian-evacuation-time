library(sf)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(rcartocolor)
library(ggspatial)
library(terra)
library(tidyterra)
library(ggpubr)
library(ggrepel)
ff <- tmpFiles(old=TRUE)
tmpFiles(old=TRUE, remove=TRUE)

# ### load functions
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_01_region_area.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_02_escape_points.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_03_minimum_distance.R')
source('~/Documents/Projects/PRSN/Data/R/Pedestrian Analysis/02_func_04_distance_grid.R')

# path inputs
path.in <- '~/Documents/Projects/GIS/'
path.r <- paste0(path.in,'Raster/DEM/')
path.v <- paste0(path.in,'Vector/')

crs = 'EPSG:4326'

pr <- vect(paste0(path.v,'PR/Municips/PR.shp'))
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
gg.1 = ggplot() +
  geom_spatvector(data = pr, fill = 'lightgreen', alpha=0.5) +
  geom_spatvector(data = area.evac, fill = 'yellow') +
  geom_spatvector(data = area.region, fill = 'pink') +
  geom_spatvector(data = area.study, fill = 'yellow') +
  # geom_spatvector(data = rds, size = 0.1) + 
  geom_spatvector(data = escape.pnts, color = 'blue', cex=0.8) +
  coord_sf(xlim = ext(area.region)[1:2],ylim = ext(area.region)[3:4],
           clip = 'on', expand = FALSE) +
  theme_light() +
  annotation_scale(location = "tr", width_hint = 0.10)

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/StudyRegion_W_escapePts.png'
png(filename=filename, units="in", width=6, height=3, res=400)
gg.1
invisible(dev.off())

writeVector(escape.pnts,paste0(path.v,'escape_pnts_sj.shp'),overwrite=TRUE)

dem <- rast(paste0(path.r,'dem_sanjuan.tif'))
area.region <- project(area.region,crs(dem))
r <- rast(resolution=25,crs=crs(dem),extent=ext(area.region))
dem <- resample(dem,r)
# writeRaster(dem, paste0(path.r,'dem_sanjuan.tif'), overwrite=TRUE)

gg.dem <- ggplot() +
  geom_spatraster(data = dem) +
  geom_spatvector(data = area.region, fill = NA) +
  scale_fill_whitebox_c(palette = "deep") +
  coord_sf(xlim = ext(area.region)[1:2],ylim = c(264000,ext(area.region)[4]),
           clip = 'on', expand = FALSE) +
  theme_light() +
  theme(legend.position = c(0.805,0.85), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=5),
        legend.text = element_text(size=5)) +
  labs(fill='Elevation (m)')
gg.dem

dem.rds <- rast(paste0(path.in,'Raster/dem2/dem_sanjuan.tif'))

gg.dem.rds <- ggplot() +
  geom_spatraster(data = dem.rds) +
  geom_spatvector(data = area.region, fill = NA) +
  scale_fill_whitebox_c(palette = "deep") +
  coord_sf(xlim = ext(area.region)[1:2],ylim = c(264000,ext(area.region)[4]),
           clip = 'on', expand = FALSE) +
  theme_light() +
  theme(legend.position = c(0.805,0.85), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=5),
        legend.text = element_text(size=5)) +
  labs(fill='Elevation (m)')
gg.dem.rds

pl <- ggarrange(gg.dem,gg.dem.rds,
                heights=2,font.label=0.9,
                ncol = 1, nrow = 2)

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/DEM_mask.png'
png(filename=filename, units="in", width=7, height=4, res=400)
pl
invisible(dev.off())


e <- vect(paste0(path.v,'extent_osj.shp'))
e <- project(e,crs(dem))
dem.rds.crop <- crop(dem.rds,e)
# escape.pnts <- project(escape.pnts,crs(dem))
# escape.pnts.crop <- crop(escape.pnts,e)
set.seed(5)
# escape.pnts.crop <- sample(escape.pnts.crop,5)
# writeVector(escape.pnts.crop,paste0(path.v,'escape_pnts_sj_sample.shp'),overwrite=TRUE)
escape.pnts.crop <- vect(paste0(path.v,'escape_pnts_sj_sample.shp'))

# # roads
rds <- project(rds,crs(dem))
rds.crop <- crop(rds,e,ext=TRUE)
rds.pnts <- as.points(rds.crop) 
rds.pnts <- crds(rds.pnts,df=TRUE)
rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem))
rds.pnts.1 <- rds.pnts[1]

cs <- create_slope_cs(dem.rds.crop)

lc.paths <- create_lcp(cs,origin=rds.pnts.1,escape.pnts.crop)
lc.paths <- project(lc.paths,crs(dem))
lc.paths <- crop(lc.paths,e)

escape.pnts.crop$Distance <- round(c(perim(lc.paths),NA))


area.study <- project(area.study,crs(dem))
gg.lcp <- ggplot() +
  geom_spatvector(data = project(pr,crs(dem)), fill = 'lightgreen', alpha=0.5) +
  geom_spatvector(data = area.study, fill = 'white') +
  geom_spatraster(data = dem.rds.crop) +
  geom_spatvector(data = lc.paths) +
  geom_spatvector(data = rds.pnts.1, color = 'black', cex=2) +
  geom_spatvector(data = escape.pnts.crop, color = 'blue', cex=2) +
  geom_spatvector_text(data = escape.pnts.crop, aes(label = Distance),
                       fontface='bold',size=3,color='red', 
                       vjust=-2, hjust=1) +
  scale_fill_whitebox_c(palette = "deep") +
  coord_sf(xlim = ext(dem.rds.crop)[1:2],ylim = ext(dem.rds.crop)[3:4],
           clip = 'on', expand = FALSE) +
  theme_light() +
  theme(legend.position = c(0.795,0.90), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=5),
        legend.text = element_text(size=5),axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(fill='Elevation (m)') +
  annotation_scale(location = "bl", width_hint = 0.10)
gg.lcp

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/lcp.png'
png(filename=filename, units="in", width=5, height=3.7, res=400)
gg.lcp
invisible(dev.off())

# # coordinate reference system
# crs <- 'epsg:3920'
# 
# # evacuation area
# area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))
# 
# # roads
# rds <- vect(paste0(path.v,'road_pr.shp'))
# rds <- project(rds,crs)
# rds <- rds[!(rds$highway %in% c('track','path','service',
#                                 'footway','cycleway')),]
# 
# # evacuation grid polygon
# grid.evac <- vect(paste0(path.v,'poly_grid.shp'))
# grid.evac <- project(grid.evac,crs)
# 
# dem <- rast(paste0(path.r,'dem2/dem_sanjuan.tif'))
# 
# # municipality-specific evac zone
# area.evac <- project(area.evac,crs(dem))
# area.study <- area.evac[area.evac$Municipio == 'San Juan',]
# 
# #calculate region area
# area.region <- region_area(area.evac,area.study)
# 
# dem <- crop(dem,area.region,mask=TRUE)
# r <- rast(resolution=1,crs=crs(dem),extent=ext(area.region))
# 
# dem <- resample(dem,r)
# 
# # roads
# rds <- project(rds,crs(dem))
# rds <- crop(rds,area.region,ext=TRUE)
# 
# escape.pnts <- escape_points(area.evac,area.region,rds)
# escape.poly <- buffer(escape.pnts,5)
# escape.poly <- aggregate(escape.poly)
# 
# rds.grid <- buffer(rds, 5)
# rds.grid <- crop(rds.grid,area.region)
# rds.grid <- rbind(rds.grid,escape.poly)
# rds.grid <- aggregate(rds.grid)
# 
# # rds.slope <- crop(slope,rds.grid,mask=TRUE)
# rds.dem <- crop(dem,rds.grid,mask=TRUE)
# 
# cs <- create_slope_cs(rds.dem)
# 
# # convert the road network into points
# rds.pnts <- as.points(rds)
# rds.pnts <- crop(rds.pnts,area.study)
# rds.pnts <- crds(rds.pnts,df=TRUE)
# rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem))
# 
# # 
# set.seed(23401)
# if (length(rds.pnts) < 150){
#   fraction <- length(rds.pnts)
# } else {fraction <- 150}
# rds.pnts <- sample(rds.pnts,fraction)
# 
# # roads area grid
# grid.evac <- project(grid.evac,crs(dem))
# grid.rds <- intersect(grid.evac, buffer(rds, 5))
# grid.rds <- crop(grid.rds,area.study)
# 
# minDist.pnts <- min_dist(cs, rds.pnts, escape.pnts)

minDist.pnts <- vect(paste0(path.v,'minDist_sj.shp'))
dist.grid <- distance_grid(minDist.pnts,area.region,
                           area.study,res.1 = 10,res.2 = 1)

gg.dist <- ggplot() +
  geom_spatvector(data = project(pr,crs(dem)), fill = 'lightgreen', alpha=0.5) +
  geom_spatvector(data = area.region, fill = 'white') +
  geom_spatraster(data = dist.grid) +
  scale_fill_whitebox_c(palette = "bl_yl_rd") +
  coord_sf(xlim = ext(area.study)[1:2],ylim = ext(area.study)[3:4],
           clip = 'on', expand = FALSE) +
  theme_light() +
  theme(legend.position = c(0.805,0.90), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=5),
        legend.text = element_text(size=5)) +
  labs(fill='Distance to Safety (m)') +
  annotation_scale(location = "bl", width_hint = 0.15)
gg.dist

time.grid <- dist.grid * (1/1.22) * (1/60) 

gg.time <- ggplot() +
  geom_spatvector(data = project(pr,crs(dem)), fill = 'lightgreen', alpha=0.5) +
  geom_spatvector(data = area.region, fill = 'white') +
  geom_spatraster(data = time.grid) +
  scale_fill_whitebox_c(palette = "bl_yl_rd") +
  coord_sf(xlim = ext(area.study)[1:2],ylim = ext(area.study)[3:4],
           clip = 'on', expand = FALSE) +
  theme_light() +
  theme(legend.position = c(0.79,0.90), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=5),
        legend.text = element_text(size=5)) +
  labs(fill='Evacuation Time (min)') +
  annotation_scale(location = "bl", width_hint = 0.15)
gg.time
# 
# pl <- ggarrange(gg.dist,gg.time,
#                 heights=2,font.label=0.9,
#                 ncol = 1, nrow = 2)

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/time.png'
png(filename=filename, units="in", width=5.5, height=3, res=400)
gg.time
invisible(dev.off())


