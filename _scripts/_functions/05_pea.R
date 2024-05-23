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

pa_grid <- function(area.evac, dem, rds, escape.pnts,
                    municipality,fraction=NULL,crs, rs=1){
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
  rds.subset <- project(rds,crs(dem))
  rds.subset <- crop(rds.subset,area.region,ext=TRUE)
  
  # produce the escape pnts for the region
  escape.pnts <- project(escape.pnts,crs(dem))
  escape.pnts <- crop(escape.pnts,ext(area.region))
  
  # create escape areas within 5 meters of the escape points
  escape.poly <- buffer(escape.pnts,20)
  escape.poly <- aggregate(escape.poly)

  # merge the escape area to the roads
  escape.r <- rasterize(escape.poly,dem)
  rds.dem <- merge(dem,escape.r)
  
  # conductance matrix for least cost path analysis
  cs <- create_slope_cs(rds.dem)
  
  # create a grid evac zone
  # convert the road network into points
  grid.evac <- rast(ext=ext(area.study),
                    crs=crs(area.study),
                    resolution=50,vals=1)
  rds.buffer <- buffer(rds.subset, 5)
  grid.rds <- crop(grid.evac, rds.buffer, mask=TRUE)
  grid.rds <- as.polygons(grid.rds, aggregate=FALSE)
  grid.rds <- intersect(grid.rds, rds.buffer)
  grid.rds <- crop(grid.rds, area.study)
  
  # convert the road network into points
  rds.pnts <- centroids(grid.rds)
  rds.pnts <- crds(rds.pnts, df=TRUE)
  rds.pnts <- vect(rds.pnts, geom=c("x", "y"), crs=crs(dem))
  
  # extract a random sample from the road points

  if (is.null(fraction) == FALSE){
    if(fraction < length(rds.pnts)){
      set.seed(23401)
      rds.pnts <- sample(rds.pnts,fraction)
      print("Fraction used.")
    }
    else{
      print(paste0("Total points ",length(rds.pnts)," used."))
      }
  }

  # least cost path analysis
  minDist.pnts <- min_dist(cs, rds.pnts, escape.pnts)
  
  # crop by the study area
  minDist.pnts <- crop(minDist.pnts,buffer(area.study,10))
  
  # voronai polygons
  v <- voronoi(minDist.pnts)
  v$time <- as.numeric(v$distance) * (1/1.22) * (1/60)
  v$muni <- municipality
  v <- v[,c('muni','distance','time')]
  names(v) <- c("Municipio","DistToSafety","EvacTimeAvg")
  
  # crop to the rds.buffer
  rds.buffer <- erase(rds.buffer)
  rds.buffer <- crop(rds.buffer,area.study)
  v <- crop(v,rds.buffer)
  
  # reproject into desired crs
  minDist.pnts <- project(minDist.pnts,crs)
  v <- project(v,crs)
  
  sv_collection <- c(minDist.pnts,v)

  return(sv_collection)
}

