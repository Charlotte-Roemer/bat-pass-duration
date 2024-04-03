#############################
# Link between species and buzz #
#############################

# This script links the file containing the species ID and the buzz present in the sonotype ID 

# Conditions of association :
# same file name (.wav)
# start and end time of buzz is within the time interval of the start and end time of the species ID
# if several species were detected simultaneously, the one with the closest frequency is kept
# filters : see arg[6] and arg[7]

arg="./example files/species_ID/" # directory with outputs from species classifier. You can put several recording sessions in separate files, identified by their names. In the example, the name is "1".
arg[2]="./example files/species_list.txt" # species list
arg[3]="./example files/association_buzz_species_ID"# output directory
arg[4]= "./example files/sonotype_ID" # directory with output from sonotype classifier. You can put several recording sessions in separate files, identified by their names. In the example, the name is "1".
arg[5]=T # if T the output file will also contain buzz associated with non-bat sounds
arg[6]=0 # threshold on buzz probability of ID to filter out files with low probabilities (if 0 they are all kept)
arg[7]=0 # threshold on species probability of ID to filter out files with low probabilities (if 0 they are all kept)

library(tidyverse)
library(beepr)
library(gdata)
library(data.table)

part_fichier <- list.files(arg[4], pattern = "IdTot_TOTAL") # list sonotype files
liste_fichier <- sapply(str_split(part_fichier, "_"), `[`, 2) # list recording sessions

for (w in 1:length(liste_fichier)) { # for each recording session
  
  print("-----")
  print(w) # session
  print("-----")
  
  fichier <- str_c("export_",substr(liste_fichier[w],1,3),".csv")
  esp_tot <- fread(paste0(arg[1],fichier), check.names=T) # read species ID
  if(!"participation" %in% names(esp_tot)){
    esp_tot$participation = gsub("export_", "", gsub(".csv", "", fichier))
    colnames(esp_tot)[colnames(esp_tot) == 'tadarida_taxon'] <- 'espece'
    colnames(esp_tot)[colnames(esp_tot) == 'nom.du.fichier'] <- 'donnee'
    colnames(esp_tot)[colnames(esp_tot) == 'temps_fin'] <- 'temps_debut.1'
    colnames(esp_tot)[colnames(esp_tot) == 'tadarida_probabilite'] <- 'probabilite'
  }
  esp_part <- esp_tot[which(esp_tot$participation==liste_fichier[w]),]
  esp_part$espece <- as.factor(esp_part$espece)
  table(esp_part$espece) # species present
  esp_part$espece <- as.factor(esp_part$espece)
  
  #
  # Extraction of bats
  #
  
  # Read species list
  list_esp <- read.table(arg[2], header = T, sep = ";", quote = "", 
                         row.names = NULL, stringsAsFactors = FALSE, 
                         encoding = "UTF-8")
  
  # All names to lower cases
  list_esp$Esp <- str_to_lower(list_esp$Esp)
  esp_part$espece <- str_to_lower(esp_part$espece)
  
  esp_part <- inner_join(esp_part, list_esp[,c("Esp","Group")], by = c("espece"="Esp"))
  
  #
  # Import sonotype dataset and extract buzzes
  #
  
  sonotyp <- fread(paste0(arg[4],"/", part_fichier[w]), fill = T) # read sonotypes ID
  sonotyp <- as.data.frame(sonotyp) 
  
  buzz <- subset(sonotyp, sonotyp$SpMaxF2=="buzz") # select lines with buzzes
  buzz <- unique(buzz) # avoids non-unique in case of error in sonotype ID
  buzz <- buzz[which(buzz$buzz>=arg[6]),] # filter low probabilities if specified
  buzz$Group.1 <- gsub(".wav", x=buzz$Group.1, replacement = "")
  
  #
  # Link between both files: with species produced a buzz?
  #
  
  if (!(nrow(buzz)==0)) {
    init=cbind(buzz, temps_debut=NA, temps_debut.1 = NA, espece = NA, Group = NA, 
               probabilite = NA, frequence = NA, diff=NA) # initialisation of dataframe
    buzz_id <- init[0,]
    rm(init)
    
    for (i in 1:nrow(buzz)) {
      
      esp_part_i=subset(esp_part,(esp_part$donnee==buzz$Group.1[i])&
                          (esp_part$temps_debut<=buzz$Tstart[i])&
                          (esp_part$temps_debut.1>=buzz$Tend[i])) # 3 conditions for association
      if (nrow(esp_part_i)!=0) { # if a correspondance if found, then:
        esp_part_i$diff <- abs(buzz$FreqM[i] - esp_part_i$frequence) # frequency difference between buzz and species
        ident <- esp_part_i[which.min(esp_part_i$diff),] # keep lowest difference
        temp <- cbind(buzz[i,], temps_debut=ident$temps_debut, temps_debut.1 = ident$temps_debut.1,
                      espece = ident$espece, Group = ident$Group, probabilite = ident$probabilite,
                      frequence = ident$frequence, diff = ident$diff)
        buzz_id <-rbind(buzz_id, temp)
      }
      print(i)
    }  
    
    rm(esp_part_i)
    
    #
    # Final output
    #
    
    buzz_id_filter <- subset(buzz_id, buzz_id$probabilite>=arg[7])
    
    if (arg[5]==T) {
      write.csv(buzz_id_filter, file = paste0(arg[3],"/buzz_id_", liste_fichier[w],".csv"), row.names = F)
    } else {
      buzz_id_bat <- subset(buzz_id_filter, buzz_id$Group=="bat")
      write.csv(buzz_id_bat, file = paste0(arg[3],"buzz_id_", liste_fichier[w],".csv"), row.names = F)
    }
  }
}
    