### Function: Minimum Distance
### This function calculates the minimum 'distance to safety'

# requires:
# library: terra, leastcostpath

# cs <- leastcostpath conductance matrix
# roads <- road points
# escape.pnts <- escape points/locations


min_dist <- function(cs, rds.pnts, escape.pnts){

  # loop through rds.pnts to get minimum distance from any escape.pnts
  minDist <- data.frame() # empty df
  for (i in 1:length(rds.pnts)){
    # run the lcp analysis
    lc.paths <- create_lcp(cs,origin=rds.pnts[i],escape.pnts)
    lc.paths <- perim(lc.paths) # return vector of distance
    lc.paths <- lc.paths[lc.paths != 0] # remove zero values
    lc.paths <- min(lc.paths) # calculate the minimum distance
    # add the results to minDist
    if (lc.paths != Inf){
      minDist[i,1:3] <- c(geom(rds.pnts[i])[,c('x','y')],lc.paths)
    }
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
  return(minDist)
}

