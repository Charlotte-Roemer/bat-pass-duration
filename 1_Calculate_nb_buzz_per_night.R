#############################
# Counts nb of buzzes per night #
#############################

# (Possible development: sort out buzzes with low confidence indexes)

library(tidyverse)
library(gdata)
library(data.table)

arg="./example files/association_buzz_species_ID/" # directory with outputs script 0_Associate_species_ID_with_buzz.R
arg[2]= "./example files/nb_buzz_per_night/" # output directory
arg[3] = "./f2pPF.R" # f2pPF function

# List files
part <- list.files(arg[1], full.names = T)

for (m in 1:length(part)) {
  
  buzz_id <- read.csv(part[m])
  buzz_id <- buzz_id[,-1]
  
  if (!(nrow(buzz_id)==0)) {
    
    source(arg[3]) # Load f2pPF function
    buzz_id$date <- f2pPF(buzz_id$Filename)
    buzz_id <- subset(buzz_id, buzz_id$Group=="bat")
    buzz_id <- subset(buzz_id, buzz_id$probabilite>=0.5)
    
    if(!(nrow(buzz_id)==0)){
      
      start <- round_date(min(buzz_id$date), unit = "day" )- 12*3600 # take date of the beginning of the night
      end <- round_date(max(buzz_id$date), unit = "day" )+ 12*3600 # take date of the end of the night
      seq_date <- seq(start,end, by="day")
      
      ListespP <- unique(buzz_id$espece)
      
      compt <- 1 
      espBuzz=data.frame(esp = NA, nuit=NA, nb_buzz = NA)
      
      for (i in 1:length(ListespP)) { # for each species
        esp_sel=subset(buzz_id,buzz_id$espece==ListespP[i])
        for (k in 2:length(seq_date)) { # for each date
          esp_sel_d <- subset(esp_sel, 
                              esp_sel$date >= seq_date[k-1] &
                                esp_sel$date <= seq_date[k])
          espBuzz[compt,] = cbind(esp=ListespP[i], nuit = as.character(seq_date[k-1]), nb_buzz = nrow(esp_sel_d))
          compt <- compt+1
        }
      }
      
      espBuzz$nuit <- as.POSIXct(espBuzz$nuit, tz="UTC")
      
      espBuzzfinal <- cbind(espBuzz, participation = rep(gsub(".*(_)(.*).csv", "\\2", part[m]), nrow(espBuzz)))
      write.csv(espBuzzfinal, file = paste0(arg[2], "NbBuzz_",gsub(".*(_)(.*).csv", "\\2", part[m]),".csv"), row.names = F) } 
  } else {
    print("pas de buzz")
  }
}
