library(sf)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(rcartocolor)

library(terra)
library(tidyterra)
library(ggpubr)
library(ggrepel)
ff <- tmpFiles(old=TRUE)
tmpFiles(old=TRUE, remove=TRUE)

# ### load functions
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_01_region_area.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_02_escape_points.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_03_minimum_distance.R')
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/02_func_04_distance_grid.R')

# path inputs
path.in <- '~/Documents/Projects/GIS/'
path.r <- paste0(path.in,'Raster/DEM/')
path.v <- paste0(path.in,'Vector/')

crs = 'EPSG:4326'

pr <- vect(paste0(path.v,'PR/Municips/PR.shp'))
pr <- project(pr,crs)
pr <- pr[!(pr$Municipio %in% c("Culebra","Vieques")),]
pr <- aggregate(pr)


# evacuation area
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))
area.evac <- project(area.evac,crs)
area.evac <- area.evac[!(area.evac$Municipio %in% c("Culebra","Vieques")),]

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
bb <- vect(ext(area.region), crs=area.region)

areal.imagery <- rast(paste0(path.in,'Raster/GEE/PR/greater_sj.tif'))
areal.imagery <- areal.imagery[[c(4,3,2)]]
areal.imagery <- project(areal.imagery,crs)
areal.imagery <- crop(areal.imagery, area.region,ext = TRUE)



filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/StudyRegion_SJ.png'
png(filename=filename, units="in", width=10, height=5.9, res=400)

par(fig = c(0,1,0,1))
plot(area.study, col=NA, alpha=0.3,xlim = c(-66.13,-66.01))
plotRGB(areal.imagery,add=TRUE)
plot(pr, col='black', add=TRUE,density=8,angle=45)
plot(area.region, col='yellow',add=TRUE)
legend("topright", legend=c('Safe Zone','Evacuation Zone'),  
       fill=c('black',adjustcolor('yellow',alpha.f=1)),
       density=c(20,NA), angle=c(45,NA),
       bg='white', cex=1,x.intersp=0.5)


par(fig = c(0.705,1, 0.5, 0.9), new = T)
plot(pr,col= "lightgray",alpha=0.2, lwd = 0.1,
     xlim=ext(area.evac)[1:2], ylim=ext(area.evac)[3:4],
     background='white', axes=FALSE)
plot(aggregate(area.evac),col="yellow",lwd=0.1,add=TRUE)
plot(bb, col = 'red',alpha=0.8,add=TRUE)

invisible(dev.off())