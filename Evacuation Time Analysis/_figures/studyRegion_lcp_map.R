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


# tile1 <- get_tiles(ext(c(-66.0975,-66.0775,18.4555,18.4685)), provider = "OpenStreetMap", zoom = 16, cachedir = ".")
# writeRaster(tile1, paste0(path.in,'Raster/OpenStreetMap/PR/osj_zoom16.tif'))
tile1 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/osj_zoom16.tif'))

divisor <- 1
mc <- (nrow(tile1) * ncol(tile1))/divisor

gg.lcp <- ggplot() +
  # geom_spatraster(data = bb, fill = 'blue',alpha=0.3) +
  geom_spatraster_rgb(data = tile1, maxcell=mc) + 
  # geom_spatvector(data = pr, fill = 'lightgreen') +
  geom_spatvector(data = area.region, fill = 'yellow', aes(shape = "Evacuation Zone"),
    alpha=0.2) +
  geom_spatvector(data = lc.paths, size = 0.5, color='blue') +
  geom_spatvector(data = pnts, aes(color = `Point Type`), cex=2) +
  scale_color_manual(values=c('black','blue')) +
  guides(fill = guide_legend(override.aes = list(fill = 0))) +
  geom_spatvector_text(data = escape.pnts.crop[escape.pnts.crop$Distance>1,], 
                       aes(label = paste0(Distance,'m')),
                       fontface='bold',size=3,color='black', 
                       vjust=-1.8, hjust=1) +
  coord_sf(crs = 4326, xlim = c(-66.0975,-66.0775),ylim = c(18.4554,18.4685),
           clip = 'on', expand = FALSE) +
  theme_light() +
  theme(legend.position = 'bottom', legend.direction="horizontal",
        text=element_text(size=4),legend.title = element_blank(),
        legend.text = element_text(size=6),axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text=element_text(size=5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotation_scale(location = "tr", text_cex = 0.7, width_hint = 0.10)
gg.lcp

bb <- vect(ext(pnts), crs=pnts)

# tile2 <- get_tiles(ext(area.study), provider = "OpenStreetMap", zoom = 13, cachedir = ".")
# plot(tile2)
# writeRaster(tile2, paste0(path.in,'Raster/OpenStreetMap/PR/sj_zoom12.tif'), overwrite=TRUE)
tile2 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/sj_zoom12.tif'))

insetMap <- ggplot() + 
  geom_spatraster_rgb(data = tile2, maxcell=Inf) + 
  geom_spatvector(data = pr, fill = "lightgray", alpha=0.5,show.legend = FALSE) +
  geom_spatvector(data = area.region, fill = 'yellow', show.legend = FALSE) +
  geom_spatvector(data = bb, fill = NA,
                  color = "red", lwd = 1) +
  coord_sf(crs = 4326, xlim = ext(area.study)[1:2],ext(area.study)[3:4],
           clip = 'on', expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(color = 'black',fill = NA,size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
insetMap

ggm = ggdraw() +
  draw_plot(gg.lcp) +
  draw_plot(insetMap, x = 0.72, y = 0.72, width = 0.2, height = 0.2)
ggm
filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/lcp_2.png'
png(filename=filename, units="in", width=5.2, height=3.7, res=400)
ggm
invisible(dev.off())

