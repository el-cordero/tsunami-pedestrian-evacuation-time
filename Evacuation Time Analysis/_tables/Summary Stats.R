### Summary Statistics Table

# ### load libraries
library(terra)
library(tidyterra)
tmpFiles(old=TRUE, remove=TRUE)

# path inputs
path.in <- '~/Documents/Projects/GIS/'
path.v <- paste0(path.in,'Vector/')

# coordinate reference system
crs <- 'epsg:3920'

# tsunami evacuation zone
area.evac <- vect(paste0(path.v,'poly_zono_desalojo.shp'))
area.evac <- project(area.evac,crs)

# calculate area
area.evac$area <- expanse(area.evac, unit = 'km', transform=TRUE)

# summarize by Municipio
results <- area.evac %>%
  group_by(Municipio) %>%
  summarize(area = sum(area))
results <- results[order(results$Municipio),]

# tsunami evacuation time/distance dataset
peat <- vect(paste0(path.v,'PRpeat.shp'))

# summary statistics
peat.results <- peat %>%
  group_by(Municipio) %>%
  summarize(avgDist = round(mean(DistToSafe)/1000,2), #km
            # medDist = round(median(DistToSafe)/1000,2),
            maxDist = round(max(DistToSafe)/1000,2),
            avgTime = round(mean(EvacTimeAv)),
            # medTime = round(median(EvacTimeAv)),
            maxTime = round(max(EvacTimeAv)))

# edit dataframe
peat.results <- data.frame(peat.results)
peat.results[peat.results$Municipio %in% 
               c('Humacao','Yauco','Naguabo'),-1] <- c(NA,NA,NA,NA)
missing <- data.frame(cbind(c('Ceiba', 'Vieques'),
                      c(NA,NA), c(NA,NA), c(NA,NA), c(NA,NA)))
names(missing) <- names(peat.results)
peat.results <- rbind(peat.results,missing)
peat.results <- peat.results[order(peat.results$Municipio),]

# merge summary dataframes
peat.results$Municipio == results$Municipio
peat.results$area <- round(results$area,1)

peat.results <- peat.results[,c(1,6,2:5)]

# save as csv
write.csv(peat.results, '~/Documents/Projects/PRSN/Data/R/Evacuation Time Analysis/peat_results.csv',
          row.names = FALSE)

