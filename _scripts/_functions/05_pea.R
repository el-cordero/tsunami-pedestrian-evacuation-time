### Function: Pedestrian Analysis
### Runs a PEA on files

# requires:
# library: terra, fields, leastcostpath

# area.evac <- the entire evacuation zone (zono de desalojo)
# rds <- roads
# municipality <- town or city
# dem <- dem raster
# crs <- coordinate reference system
# rs <- resolution

pa_grid <- function(area.evac, dem, rds, grid.evac, 
                    municipality,fraction,crs, rs=1){
  # municipality-specific evac zone
  area.evac <- project(area.evac,crs(dem))
  area.study <- area.evac[area.evac$Municipio == municipality,]
  
  #calculate region area
  area.region <- region_area(area.evac,area.study)

  # crop dem to region area
  dem <- crop(dem,area.region,mask=TRUE)
  
  # resample dem
  r <- rast(resolution=rs,crs=crs(dem),extent=ext(area.region))
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
  rds.dem <- crop(dem,rds.grid,mask=TRUE)
  
  # conductance matrix for least cost path analysis
  cs <- create_slope_cs(rds.dem)
  
  # convert the road network into points
  rds.pnts <- as.points(rds)
  rds.pnts <- crop(rds.pnts,area.study)
  rds.pnts <- crds(rds.pnts,df=TRUE)
  rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem))
  
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
  grid.rds$Municipio <- municipality

  # least cost path analysis
  minDist.pnts <- min_dist(cs, rds.pnts, escape.pnts)
  
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

  return(grid.rds)
}
