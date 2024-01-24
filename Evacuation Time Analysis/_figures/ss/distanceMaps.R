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
path.r <- paste0(path.in,'Raster/')
path.v <- paste0(path.in,'Vector/')

# coordinate reference system
crs <- 'epsg:3920'

# evacuation area
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))

# roads
rds <- vect(paste0(path.v,'road_pr.shp'))
rds <- project(rds,crs)
rds <- rds[!(rds$highway %in% c('track','path','service',
                                'footway','cycleway')),]

# evacuation grid polygon
grid.evac <- vect(paste0(path.v,'poly_grid.shp'))
grid.evac <- project(grid.evac,crs)

dem <- rast(paste0(path.r,'dem2/dem_sanjuan.tif'))

# municipality-specific evac zone
area.evac <- project(area.evac,crs(dem))
area.study <- area.evac[area.evac$Municipio == 'San Juan',]

#calculate region area
area.region <- region_area(area.evac,area.study)

dem <- crop(dem,area.region,mask=TRUE)
r <- rast(resolution=1,crs=crs(dem),extent=ext(area.region))

dem <- resample(dem,r)

# roads
rds <- project(rds,crs(dem))
rds <- crop(rds,area.region,ext=TRUE)

escape.pnts <- escape_points(area.evac,area.region,rds)
escape.poly <- buffer(escape.pnts,5)
escape.poly <- aggregate(escape.poly)

rds.grid <- buffer(rds, 5)
rds.grid <- crop(rds.grid,area.region)
rds.grid <- rbind(rds.grid,escape.poly)
rds.grid <- aggregate(rds.grid)

# rds.slope <- crop(slope,rds.grid,mask=TRUE)
rds.dem <- crop(dem,rds.grid,mask=TRUE)

cs <- create_slope_cs(rds.dem)

# convert the road network into points
rds.pnts <- as.points(rds)
rds.pnts <- crop(rds.pnts,area.study)
rds.pnts <- crds(rds.pnts,df=TRUE)
rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem))

# 
set.seed(23401)
if (length(rds.pnts) < 150){
  fraction <- length(rds.pnts)
} else {fraction <- 150}
rds.pnts <- sample(rds.pnts,fraction)

# roads area grid
grid.evac <- project(grid.evac,crs(dem))
grid.rds <- intersect(grid.evac, buffer(rds, 5))
grid.rds <- crop(grid.rds,area.study)

minDist.pnts <- min_dist(cs, rds.pnts, escape.pnts)
writeVector(minDist.pnts,paste0(path.v,'minDist_sj.shp'),overwrite=TRUE)

dist.grid <- distance_grid(minDist.pnts,area.region,
                           area.study,res.1 = 10,res.2 = 1)


gg.lcp <- ggplot() +
  geom_spatvector(data = area.study, fill = NA) +
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
  theme(legend.position = c(0.805,0.90), legend.direction="horizontal",
        text=element_text(size=5),legend.title = element_text(size=5),
        legend.text = element_text(size=5)) +
  labs(fill='Elevation (m)') +
  annotation_scale(location = "bl", width_hint = 0.10)
gg.lcp

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/lcp.png'
png(filename=filename, units="in", width=5, height=4, res=400)
gg.lcp
invisible(dev.off())

dist.vals <- extract(dist.grid,grid.rds,fun=min,method='simple')

# grid.rds$Distancetosafety <- as.integer(dist.vals[,2])
# 
# grid.rds <- project(grid.rds,crs)
# 
# writeVector(grid.rds,'peat.shp',overwrite=TRUE)

