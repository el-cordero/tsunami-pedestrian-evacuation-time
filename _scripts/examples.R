library(terra) 
library(fields)
library(leastcostpath)

# evacuation area
area.evac <- vect('/Users/EC13/Documents/Projects/PRSN/Data/GIS/Vector/poly_zono_desalojo.shp')

grid.evac <- vect('/Users/EC13/Documents/Projects/PRSN/Data/GIS/Vector/poly_grid.shp')

arecibo <- area.evac[area.evac$Municipio == 'Arecibo',]

# roads
rds <- vect('/Users/EC13/Documents/Projects/PRSN/Data/GIS/Vector/road_pr.shp')
rds <- rds[!(rds$highway %in% c('track','path','service',
                                'footway','cycleway')),]

plot(arecibo)
plot(grid.evac,ext=arecibo)


area.region <- aggregate(arecibo,dissolve=TRUE)
buff <- buffer(arecibo, 5000)
buff <- aggregate(buff,dissolve=TRUE)
area.region <- crop(area.region,buff)

plot(arecibo)
plot(area.region,col='yellow',add=TRUE)
plot(arecibo,add=TRUE,col='blue')

unique(area.evac$Municipio)

rds <- project(rds,area.evac)

area.evac.lines <- as.lines(area.region)
rds <- as.lines(rds)
escape <- terra::intersect(rds,area.evac.lines)
# escape <- as.points(escape)

plot(area.evac.lines,ext=arecibo)
plot(rds,add=TRUE)
plot(escape,add=TRUE)

writeVector(escape,'/Users/EC13/Documents/Projects/PRSN/Data/GIS/Vector/escape_pr.shp')
barc <- area.evac[area.evac$Municipio == 'Barceloneta',]
s <- rast(ext=ext(barc),crs=crs(area.evac),res=5)
evac.r <- rasterize(barc,s)

negative <- rast(ext=ext(barc),crs=crs(area.evac),res=5)
values(negative) <- 2
negative.mask <- mask(negative,evac.r,inverse=TRUE)

plot(negative.mask)
neg.poly <- as.polygons(negative.mask)
# There is an issue with the conversion of lines to points.
# Multipoint vector is created with certain points
# containing points with multiple parts.
escape <- crds(escape,df=TRUE)
escape <- vect(escape, geom=c("x", "y"), crs=crs(area.evac))

# crop to region extent
escape <- crop(escape,area.region,ext=TRUE)
