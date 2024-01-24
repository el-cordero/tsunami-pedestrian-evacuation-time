# ### load libraries
source('~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/01_libraries.R')

pr <- vect('~/Documents/Projects/GIS/Vector/PR/Municips/PR.shp')
pr <- aggregate(pr)
area.evac <- vect('~/Documents/Projects/GIS/Vector/poly_zono_desalojo.shp')
area.evac.agg <- aggregate(area.evac)
world <- vect(system.file("shapes/world.gpkg", package="spData"))

pr_bb <- vect(ext(pr), crs=pr)



ggm1 = ggplot() + 
  geom_spatvector(data = world, fill = "lightgray",alpha=0.4, lwd = 0.1) + 
  geom_spatvector(data = pr_bb, fill = NA, 
          color = "red", lwd = 2) +
  coord_sf(crs = 4326) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.background = element_rect(color='white'),
        panel.border = element_rect(color = "black",fill=NA,
                                    size = 1))
ggm1

# tile <- get_tiles(area.evac, provider = 'Esri.OceanBasemap', 
#                    zoom = 10, cachedir = ".")
# writeRaster(tile, paste0(path.in,'Raster/OpenStreetMap/PR/pr_zoom10_esri_ocean.tif'),
#             overwrite=TRUE)
tile <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/pr_zoom10_esri_ocean.tif'))

ggm2 = ggplot() + 
  geom_spatraster_rgb(data = tile, maxcell = Inf) + 
  # geom_spatvector(data = pr, fill = "lightgray", alpha=0.5,show.legend = FALSE) +
  geom_spatvector(data = area.evac.agg, aes(fill = Municipio), fill = 'red', 
          show.legend = FALSE) +
  # geom_sf_label(data= area.evac, aes(label = Municipio)) + 
  coord_sf(crs = 4326,xlim = c(-67.3,-65.05),ylim = c(18.6,17.79),
           clip = 'on', expand = FALSE) +
  labs(title = "(a) Main Island - Puerto Rico") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title=element_text(face='bold', size=12))+
  annotation_scale(location = "bl", width_hint = 0.20) 
ggm2

# tile2 <- get_tiles(area.evac[area.evac$Municipio=='Vieques',], provider = 'Esri.OceanBasemap',
#                    zoom = 12, cachedir = ".")
# writeRaster(tile2, paste0(path.in,'Raster/OpenStreetMap/PR/vieques_zoom12_esri_ocean.tif'),
#             overwrite=TRUE)
tile2 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/vieques_zoom12_esri_ocean.tif'))

ggm3 = ggplot() + 
  geom_spatraster_rgb(data = tile2, maxcell = Inf) + 
  # geom_spatvector(data = pr, fill = "lightgray", alpha=0.5,show.legend = FALSE) +
  geom_spatvector(data = area.evac.agg, aes(fill = Municipio), fill = 'red', 
                  show.legend = FALSE) +
  # geom_sf_label(data= area.evac, aes(label = Municipio)) + 
  coord_sf(crs = 4326,xlim = c(-65.6,-65.25),ylim = c(18.07,18.175),
           clip = 'on', expand = FALSE) + 
  labs(title = "(c) Vieques") + 
  theme_void() +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(face='bold', size=12))+
  annotation_scale(location = "br", width_hint = 0.20) 
ggm3

tile3 <- get_tiles(area.evac[area.evac$Municipio=='Culebra',], provider = 'Esri.OceanBasemap',
                   zoom = 12, cachedir = ".")
writeRaster(tile3, paste0(path.in,'Raster/OpenStreetMap/PR/culebra_zoom12_esri_ocean.tif'),
            overwrite=TRUE)
tile3 <- rast(paste0(path.in,'Raster/OpenStreetMap/PR/culebra_zoom12_esri_ocean.tif'))

ggm4 = ggplot() + 
  geom_spatraster_rgb(data = tile3, maxcell = Inf) + 
  geom_spatvector(data = area.evac.agg, aes(fill = Municipio), fill = 'red', 
                  show.legend = FALSE) +
  # geom_sf_label(data= area.evac, aes(label = Municipio)) + 
  coord_sf(crs = 4326,xlim = c(-65.40,-65.218),ylim = c(18.27,18.365),
           clip = 'on', expand = FALSE) + 
  labs(title = "(b) Culebra") + 
  theme_void() +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(face='bold', size=12))+
  annotation_scale(location = "bl", width_hint = 0.20)
  
ggm4


ggm = ggdraw() +
  draw_plot(ggm2) +
  draw_plot(ggm3, x = 0.69, y = 0.14, width = 0.3, height = 0.3) +
  draw_plot(ggm4, x = 0.74, y = 0.415, width = 0.29, height = 0.29) +
  draw_plot(ggm1, x = 0.84, y = 0.70, width = 0.15, height = 0.15)
ggm

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/StudyArea.png'
png(filename=filename, units="in", width=8, height=3.9, res=400)
ggm
# Close png creation
invisible(dev.off())



