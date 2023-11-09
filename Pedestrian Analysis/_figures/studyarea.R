library(sf)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(spData)
library(ggspatial)

pr <- read_sf('~/Documents/Projects/GIS/Vector/PR/Municips/PR.shp')
desa <- read_sf('~/Documents/Projects/GIS/Vector/poly_zono_desalojo.shp')
world <- st_read(system.file("shapes/world.gpkg", package="spData"))

pr <- st_transform(pr, crs = 4326)
desa <- st_transform(desa, crs = 4326)
world <- st_transform(world, crs = 4326)

pr_bb = st_as_sfc(st_bbox(pr))

ggm1 = ggplot() + 
  geom_sf(data = world, fill = "lightgray",alpha=0.4, lwd = 0.1) + 
  geom_sf(data = pr_bb, fill = NA, 
          color = "red", lwd = 2) +
  # coord_sf(xlim = c(-110,-24.15),ylim = c(58.6,-25.86),
  #          clip = 'on', expand = FALSE) + 
  theme_void() +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))
ggm1

ggm2 = ggplot() + 
  geom_sf(data = pr, fill = "lightgray", alpha=0.5,show.legend = FALSE) +
  geom_sf(data = desa, aes(fill = Municipio), fill = 'yellow', 
          show.legend = FALSE) +
  # geom_sf_label(data= desa, aes(label = Municipio)) + 
  coord_sf(xlim = c(-67.3,-65.15),ylim = c(18.6,17.79),
           clip = 'on', expand = FALSE) + 
  theme_bw()+
  annotation_scale(location = "bl", width_hint = 0.20) 
ggm2


ggm = ggdraw() +
  draw_plot(ggm2) +
  draw_plot(ggm1, x = 0.75, y = 0.18, width = 0.2, height = 0.2)

filename <- '~/Documents/Projects/PRSN/PA/Documentation/Written/Markdown/media/StudyArea.png'
png(filename=filename, units="in", width=7, height=3.2, res=300)
ggm
# Close png creation
invisible(dev.off())



