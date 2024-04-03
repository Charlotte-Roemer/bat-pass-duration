###
# Calculation of bat pass duration
###

# This script calculates bat pass durations : aggregates
# bat passes when then follow each other by less than x seconds
# (x to specify in arg[4]) for each species

# OUTPUT : 
# Bat pass number
# Species
# TempsDebutSequence : start time of bat pass
# TempsFinSequence : end time of bat pass
# DureeSequence : duration of bat pass in seconds

arg = "./example files/part_sel.txt" #list of recording sessions: one row for each session's name. In the example, the name is "1"
arg[2] = "./example files/species_ID/" # directory with outputs from species classifier
arg[3] = "./f2pPF.R" # f2pPF function
arg[4] = as.numeric(2) # how many seconds to separate 2 different bat passes (default is 2)
arg[5] = "./example files/species_list.txt" # species list
arg[6] = "./example files/bat_pass_duration/bat_pass_duration/" # output directory for bat pass duration
arg[7] = "./example files/bat_pass_duration/standard_deviation/" # output directory for standard deviation


library(tidyverse)
library(data.table)

source(arg[3])

part_sel <- read.table(arg[1])$V1


for (i in 1:length(part_sel)) { # for each recording session
  
  # Read species ID
  fichier <- str_c(arg[2], "export_", substr(part_sel[i], 1, 3), ".csv")
  esp_tot <- fread(fichier, check.names = T)
  esp_tot = as.data.frame(esp_tot)
  
  if(!"participation" %in% names(esp_tot)){
    esp_tot$participation = gsub("export_", "", gsub(".csv", "", basename(fichier)))
    colnames(esp_tot)[colnames(esp_tot) == 'tadarida_taxon'] <- 'espece'
    colnames(esp_tot)[colnames(esp_tot) == 'nom.du.fichier'] <- 'donnee'
    colnames(esp_tot)[colnames(esp_tot) == 'temps_fin'] <- 'temps_debut.1'
    colnames(esp_tot)[colnames(esp_tot) == 'tadarida_probabilite'] <- 'probabilite'
  }
  
  esp_part <- esp_tot[which(esp_tot$participation == part_sel[i]), ]
  
  # Add dates with f2pPF function
  esp_part$date_esp <- f2pPF(esp_part$donnee)
  esp_part = esp_part[order(esp_part$date_esp), ]
  
  # Add date of the recording session
  esp_part$Date_Nuit= ifelse(str_sub(esp_part$donnee,-10,-10)==0, # if after midnight, else...
                           as.character(as.Date(paste(str_sub(esp_part$donnee,-19,-16),
                                                      str_sub(esp_part$donnee,-15,-14),
                                                      str_sub(esp_part$donnee,-13,-12),
                                                      sep="-"))-1),
                           as.character(as.Date(paste(str_sub(esp_part$donnee,-19,-16),
                                                      str_sub(esp_part$donnee,-15,-14),
                                                      str_sub(esp_part$donnee,-13,-12),
                                                      sep="-"))))
  
  # Keep only bats
  list_esp <- read.table(arg[5], header = T, sep = ";", quote = "", row.names = NULL,
    stringsAsFactors = FALSE, encoding = "UTF-8")
  
  # all names in low case for compatibility
  list_esp$Esp <- str_to_lower(list_esp$Esp)
  esp_part$espece <- str_to_lower(esp_part$espece)
  esp_part <- inner_join(esp_part, list_esp[, c("Esp", "Group")], by = c("espece" = "Esp"))
  
  # Keep only bats
  esp_part <- subset(esp_part, esp_part$Group == "bat")
  
  if (nrow(esp_part) == 0) { # if no bat observation, no need to run the script
    print("pas d'obs")
  } else {
    ListespP = unique(esp_part$espece) # list species in recording session
    esp_part$NumeroSequence = NA #initialisation of the variable bat passes number
    
    compteur = 0
    espSeqAggr = data.frame()
    for (j in 1:length(ListespP)) {
      espi = subset(esp_part, esp_part$espece == ListespP[j]) # select  species
      compteur = compteur + 1 # each time we change species we change bat pass number
      print(paste(ListespP[j], nrow(espi), Sys.time())) # miscellaneous info
      if (nrow(espi) == 1) { # if only one file for the species
        espi$NumeroSequence = compteur
      } else { # if several files for the species
        for (k in 1:(nrow(espi) - 1)) { # comparison of one line with the next one
          espi$NumeroSequence[k] = compteur
          TimeEnd = espi$date_esp[k] + espi$temps_debut.1[k] # end time of bat pass
          TimeStartNext = espi$date_esp[k + 1] + espi$temps_debut[k + 1] #  start time on next line
          if (TimeStartNext > (TimeEnd + as.numeric(arg[4]))) {
            # if more than x secondes of silence between this bat pass and the next one, it is a new bat pass
            compteur = compteur + 1
          }
        }
        espi$NumeroSequence[nrow(espi)] = compteur # fill info for the last line
      }
      espSeqAggr = rbind(espSeqAggr, espi) # add species info to the final table
    }
    length(unique(espSeqAggr$NumeroSequence)) # total number of bat passes
    aggregate(espSeqAggr$NumeroSequence,
      by = list(espSeqAggr$espece),
      FUN = function(x)
        (length(unique(x)))) # summary of number of bat passes by species
    table(espSeqAggr$espece) # number of files by species
    
    # Bat pass duration
    
    especeSequence = aggregate(espSeqAggr$espece,
                               by = list(espSeqAggr$NumeroSequence),
                               unique)
    
    espSeqAggr$TimeStart = espSeqAggr$date_esp + espSeqAggr$temps_debut
    espSeqAggr$TimeEnd = espSeqAggr$date_esp + espSeqAggr$temps_debut.1
    
    TempsDebutSequence = aggregate(espSeqAggr$TimeStart,
                                   by = list(espSeqAggr$NumeroSequence),
                                   min)
    TempsFinSequence = aggregate(espSeqAggr$TimeEnd, 
                                 by = list(espSeqAggr$NumeroSequence),
                                 max)
    DureeSequence = as.numeric(TempsFinSequence$x - TempsDebutSequence$x)
    
    
    ##
    # Final file for bat pass duration
    ##
    
    fichier_final <- data.frame(NumeroSequence = unique(espSeqAggr$NumeroSequence),
        Espece = especeSequence[, 2],
        TempsDebutSequence = TempsDebutSequence[, 2],
        TempsFinSequence = TempsFinSequence[, 2],
        DureeSequence,
        Nuit = unique(data.frame(espSeqAggr$NumeroSequence, espSeqAggr$Date_Nuit))[,2])
    
    aggregate(fichier_final$DureeSequence,
              list(fichier_final$Espece),
              mean)
    
    write.csv(fichier_final, paste0(arg[6], "duree_seq_", part_sel[i], ".csv"),
              row.names = F)
    
    ##
    # Standard deviation of the time difference between two consecutive events
    ##
    
    # Looping trough date
    
    start <- round_date(min(fichier_final$TempsDebutSequence), unit = "day") - 12 * 3600 # take date at the beginning of the night
    end <- round_date(max(fichier_final$TempsDebutSequence), unit = "day") + 12 * 3600 # take date at the end of the night
    
    seq_date <- seq(start, end, by = "day") # create one sequence of date per day
    
    # Initialise data frame
    compt <- 1 
    espVar = data.frame(esp = NA, nuit = NA, st_deviation = NA, n = NA
    )
    
    for (l in 1:length(ListespP)) {
      # for each species
      esp_sel = subset(fichier_final, fichier_final$Espece == ListespP[l])
      for (m in 2:length(seq_date)) {
        # for each date
        esp_sel_d <- subset(esp_sel, esp_sel$TempsDebutSequence >= seq_date[m - 1] & esp_sel$TempsFinSequence <= seq_date[m])
        if (nrow(esp_sel_d) < 3) {
          espVar[compt, ] = c(esp = ListespP[l],
            nuit = as.character(seq_date[m - 1]),
            st_deviation = 0,
            n = nrow(esp_sel_d))
          compt <- compt + 1
        } else {
          diff <- vector()
          for (n in 2:nrow(esp_sel_d)) {
            # for each selected row
            diff[n - 1] <- esp_sel_d$TempsDebutSequence[n] - esp_sel_d$TempsFinSequence[n - 1]
          }
          espVar[compt, ] = cbind(esp = ListespP[l], nuit = as.character(seq_date[m - 1]),
            st_deviation = sd(diff), n = nrow(esp_sel_d))
          rm(diff)
          compt <- compt + 1
        }
      }
    }
    
    espVar$nuit <- as.POSIXct(espVar$nuit, tz = "UTC")
    
    write.csv(espVar, paste0(arg[7], "var_seq_", part_sel[i], ".csv"), row.names = F)
    
  }
}
