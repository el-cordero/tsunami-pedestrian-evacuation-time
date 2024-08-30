peat.results <- read.csv('~/Documents/Projects/PRSN/Tsunami Pedestrian Evacuation Time/peat_results.csv',header=TRUE)
names(peat.results) <- c('Municipality','Area (km-sq)','Mean Distance (km)',
               'Max Distance (km)', 'Mean Time (min)','Max Time (min)')
write.csv(peat.results,'~/Documents/Projects/PRSN/Tsunami Pedestrian Evacuation Time/peat_results_submission.csv')
