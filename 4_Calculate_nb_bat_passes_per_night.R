#
# Number of bat passes per night and species
#

library(data.table)
library(tidyverse)

arg = "./example files/bat_pass_duration/bat_pass_duration" # output from 2_Calculate_bat_pass_duration_and_standard_deviation.R
arg[2] = "./example files/nb_pat_passes_per_night" # output directory

liste <- list.files(arg[1], full.names = T)

for (i in 1:length(liste)) {
  
  espContact=data.frame(esp = NA, nuit=NA, nb_contact = NA, somme_duree=NA,  participation=NA)
  dur_part <- fread(liste[i])
  start <- round_date(min(as.POSIXct(dur_part$TempsDebutSequence)), unit = "day" )- 12*3600 
  end <- round_date(max(as.POSIXct(dur_part$TempsFinSequence)), unit = "day" )+ 12*3600
  seq_date <- seq(start,end, by="day")
  
  listeEsp <- unique(dur_part$Espece)
  
  compt <- 1
  
  for (y in 1:length(listeEsp)) {
    dur_part_sel <- subset(dur_part, dur_part$Espece==listeEsp[y])
  
    for (k in 2:length(seq_date)) { # for each date
      dur_part_sel_d <- subset(dur_part_sel, dur_part_sel$TempsDebutSequence >= seq_date[k-1] & dur_part_sel$TempsFinSequence <= seq_date[k])
      espContact[compt,] = cbind(esp=listeEsp[y], nuit = as.character(seq_date[k-1]), nb_contact = length(unique(dur_part_sel_d$NumeroSequence)),
                                 somme_duree = sum(dur_part_sel_d$DureeSequence), participation=substr(basename(liste[i]), 11, nchar(basename(liste[i]))-4))
      compt <- compt+1
    }
  }
  
  write.csv(espContact, paste0(arg[2], "/", "NbContact_", substr(basename(liste[i]), 11, nchar(basename(liste[i]))-4), ".csv"), row.names = F)
}