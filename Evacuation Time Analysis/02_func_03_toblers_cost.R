### Function: Tobler's Cost
### This function calculates Tobler's Cost based off elevation and
### slope. Provided in m/s.

# requires:
# library: terra

# dem <- dem raster
# area.study <- study area

toblers_cost <- function(dem,area.study,rs){
  
  # mask the dem to the study area
  dem <- crop(dem,area.study, mask=TRUE)
  r <- rast(resolution=rs,crs=crs(dem),extent=ext(area.study))
  dem <- resample(dem,r)
  
  # Calculate the slope
  slope <- terrain(dem,'slope',unit='degrees')
  
  # Perform Tobler's Cost calculation
  cost.toblers <- (6 * exp(-3.5 * abs(tan((slope*pi)/180) + 0.05)))
  cost.toblers <- (cost.toblers / rs) * 1000
  
  # Extract the first layer
  cost.toblers <- cost.toblers['lyr1']
  
  cost.toblers <- crop(cost.toblers,area.study,mask=TRUE)
  return(cost.toblers)
}
