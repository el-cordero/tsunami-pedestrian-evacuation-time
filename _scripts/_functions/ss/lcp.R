library(igraph)
x <- cs
origin <- rds.pnts
destination <- escape.pnts
cost_distance <- FALSE
check_locations <- TRUE

origin <- project(origin,crs(cs_rast))
destination <- project(destination,crs(cs_rast))
destination <- crop(destination, cs_rast, ext=TRUE) 

if(check_locations) { 
  check_locations(x, origin)
  check_locations(x, destination)
}

cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)

from_coords <- get_coordinates(origin)
to_coords <- get_coordinates(destination)

from_cell <- terra::cellFromXY(cs_rast, from_coords[1,, drop = FALSE])
to_cell <- terra::cellFromXY(cs_rast, to_coords)

cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)

igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)

lcp_graph <- igraph::shortest_paths(cm_graph, from = from_cell, to = to_cell, mode = "out", algorithm = "dijkstra")

lcps <- lapply(lcp_graph$vpath, FUN = function(i) { 
  
  lcp_xy <- terra::xyFromCell(cs_rast, as.integer(i))
  lcp <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(lcp_xy)), crs = x$crs)
  return(lcp)
}
)

lcps <- do.call(rbind, lcps)

if(!is.function(x$costFunction)) { 
  lcps$costFunction <- x$costFunction
} else if (is.function(x$costFunction)) { 
  lcps$costFunction <- deparse(body(x$costFunction)[[2]])
}

lcps$fromCell <- from_cell
lcps$toCell <- to_cell

if (cost_distance) {
  lcps$cost <- NA
  for(i in 1:length(to_cell)) { 
    lcps$cost[i] <- igraph::distances(graph = cm_graph, v = from_cell, to = to_cell[i], mode = "out", algorithm = "dijkstra")
  }
}

if(sum(to_cell %in% from_cell) != 0) { 
  message(sum(to_cell %in% from_cell), " least-cost paths could not be calculated from origin to destination as these share the same location")
}

lcps <- lcps[!is.na(sf::st_is_valid(lcps)),]

if(inherits(origin, "SpatVector") & inherits(destination, "SpatVector")) { 
  lcps <- terra::vect(lcps)
}
