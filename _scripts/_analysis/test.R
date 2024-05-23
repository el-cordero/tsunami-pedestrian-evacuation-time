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

# evacuation grid
grid.evac <- vect(paste0(path.v,'poly_grid.shp'))

# roads
rds <- vect(paste0(path.v,'road_pr.shp'))
rds <- rds[!(rds$highway %in% c('track','path','service',
                                'footway','cycleway')),]

# create a file list of dems
dem <- rast(paste0(path.r,'dem_aguada.tif'))

# What fraction of road points to use?
fraction <- 1000 # 1000 was used for all municipalities except
# fraction <- 700 # cabo rojo
# fraction <- 1500 # arecibo?

# municipality-specific evac zone
area.evac <- project(area.evac,crs(dem))
area.study <- area.evac[area.evac$Municipio == 'Aguada',]

#calculate region area
area.region <- region_area(area.evac,area.study)

# crop dem to region area
dem <- crop(dem,area.region)

# resample dem
r <- rast(resolution=5,crs=crs(dem),extent=ext(area.region))
dem <- resample(dem,r)

# roads
rds <- project(rds,crs(dem))
rds <- crop(rds,area.region,ext=TRUE)

# produce the escape pnts
escape.pnts <- escape_points(area.evac,area.region,rds)

# create escape areas within 5 meters of the escape points
escape.poly <- buffer(escape.pnts,5)
escape.poly <- aggregate(escape.poly)

# buffer the roads and combine with escape areas
rds.buffer <- buffer(rds, 5)
rds.grid <- crop(rds.buffer,area.region)
rds.grid <- rbind(rds.grid,escape.poly)
rds.grid <- aggregate(rds.grid)

# crop dem to roads
rds.dem <- crop(dem,rds.grid)

# conductance matrix for least cost path analysis
cs <- create_slope_cs(rds.dem)

# convert the road network into points
rds.pnts <- as.points(rds)
rds.pnts <- crop(rds.pnts,area.study)
rds.pnts <- crds(rds.pnts,df=TRUE)
rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem))

fraction <- 200
# extract a random sample from the road points
set.seed(23401)
if (length(rds.pnts) < fraction){
    fraction <- length(rds.pnts)
} else {fraction <- fraction}
rds.pnts <- sample(rds.pnts,fraction)

# create a roads from the rds buffer and grid evac zone
grid.evac <- project(grid.evac,crs(dem))
grid.rds <- intersect(grid.evac, rds.buffer)
grid.rds <- crop(grid.rds,area.study)
grid.rds$Municipio <- 'Aguada'


# least cost path analysis
Sys.time()
min_dist_vals <- c()
lc_paths_list <- c() # empty df
for (i in 1:nrow(rds.pnts)){
    # buffer_area <- (expanse(area.study)/(1000*1000)/10 + 1.5) * 1000    
    # point_buffer <- buffer(rds.pnts[i], buffer_area)
    # near_escape_pnts <- crop(escape.pnts,point_buffer)
    near_escape_pnts <- escape.pnts
    for (j in 1:nrow(near_escape_pnts)){
        lc_path <- calculate_lc_path(cs,rds.pnts[i],near_escape_pnts[j])
        lc_paths_list <- c(lc_paths_list,lc_path)
    }
    min_dist <- calculate_min_dist(lc_paths_list)
    min_dist_vals <- c(min_dist_vals,min_dist)
}
Sys.time()
test_vals <- min_dist_vals

x <- 0:55 * (1000*1000)
y <- 0:55
buffer_area <- (x/(1000*1000)/10 + 1.5) * 1000

plot(y,buffer_area,type='l')
min_dist

plot(buffer(rds.pnts[1],2000))
plot(rds.pnts[1],add=TRUE)
plot(rds.grid,add=TRUE)
plot(terra::project(lc_paths,rds.pnts),'toCell',add=TRUE,col='blue')
plot(area.study,add=TRUE)



for (i in 1:nrow(escape.pnts)){
    lc.paths <- tryCatch(
        {
        create_lcp(x = cs, origin = rds.pnts[1],
        destination = escape.pnts[i],check_locations = TRUE)
    },  error = function(e) {return(NULL)}
    )



    if (is.null(lc.paths) == FALSE){
        minDist <- vect(c(minDist, lc.paths))
        minDist <- perim(minDist) # return vector of distance
        minDist <- minDist[minDist != 0] # remove zero values
        minDist <- min(minDist) # calculate the minimum distance
    
        # add the results to minDist
    if (minDist != Inf){
        minDist.df[i,1:3] <- c(geom(rds.pnts[i])[,c('x','y')],minDist)
    }
    }
    print(minDist)


}


    # add the results to minDist
    if (lc.paths != Inf){
        minDist[i,1:3] <- c(geom(rds.pnts[i])[,c('x','y')],lc.paths)
    }

# data cleaning
minDist <- na.omit(minDist) # remove NA rows
names(minDist) <- c('x','y','distance')
minDist$type <- 'road'

# add escape.pnts to minDist (distance = 0)
escapeDist <- data.frame(geom(escape.pnts)[,c('x','y')],
                        distance = 0, type = 'escape')
minDist <- rbind(minDist, escapeDist)

# return a spatvector object
minDist <- vect(minDist, geom=c("x", "y"), crs=crs(rds.pnts))

# interpolation of minimum distance to a raster
dist.grid <- distance_grid(minDist.pnts,area.region,
                            area.study,res.1 = 10,res.2 = rs)

# extract the points from the raster
dist.vals <- extract(dist.grid,grid.rds,fun=min,method='simple')
grid.rds$DistToSafety <- as.integer(dist.vals[,2]) # meters
grid.rds[is.na(grid.rds$DistToSafety) == TRUE,] <- 0

# calculate the evacuation time
grid.rds$EvacTimeAvg <- grid.rds$DistToSafety * (1/1.22) * (1/60) # minutes

grid.rds <- grid.rds[,c("Municipio","DistToSafety","EvacTimeAvg")]

# reproject into desired crs
grid.rds <- project(grid.rds,crs)


