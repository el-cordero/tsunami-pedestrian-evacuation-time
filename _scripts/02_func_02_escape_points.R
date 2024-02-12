### Function: Escape Points
### This function intersects the roads network with the borders of the
### study area, then converts the intersection locations into points. 
### Essentially, it discovers escape/exit points

# requires:
# library: terra

# network <- roads
# area.region <- study region (greater area)

escape_points <- function(area.evac,area.region,network){
  area.evac <- aggregate(area.evac)
  area.evac <- as.lines(area.evac)
  network <- as.lines(network)
  escape <- terra::intersect(network,area.evac)
  # escape <- as.points(escape)
  
  # There is an issue with the conversion of lines to points.
  # Multipoint vector is created with certain points
  # containing points with multiple parts.
  escape <- crds(escape,df=TRUE)
  escape <- vect(escape, geom=c("x", "y"), crs=crs(area.evac))
  
  # crop to region extent
  escape <- crop(escape,area.region,ext=TRUE)
  
  return(escape)
}
