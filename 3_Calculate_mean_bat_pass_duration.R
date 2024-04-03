###
# Mean bat pass duration
###

arg = "./example files/bat_pass_duration/bat_pass_duration/" #list of bat pass durations calculated
arg[2] = "./example files/bat_pass_duration/mean_bat_pass_duration/" #output

library(tidyverse)
library(data.table)

list_par = list.files(arg[1], full.names = T)

Table_tot = data.frame()
for (i in 1:length(list_par)){ # for each recording session
  fichier_i = fread(list_par[i])
  
  Table_Night = data.frame()
  for (j in 1:length(names(table(fichier_i$Nuit)))){ # for each night
    night = names(table(fichier_i$Nuit))[j]
    fichier_i_nuit = subset(fichier_i, fichier_i$Nuit==night)
    
    Table_Sp = data.frame()
    for(k in 1:length(names(table(fichier_i$Espece)))){ # for each species
      Species = names(table(fichier_i$Espece))[k]
      fichier_i_nuit_sp = subset(fichier_i_nuit, fichier_i_nuit$Espece == Species)
      Temp = aggregate(x = fichier_i_nuit_sp$DureeSequence, 
                                   by = list(nuit = fichier_i_nuit_sp$Nuit ),
                                   FUN="mean")
      names(Temp)[which(names(Temp)=="x")]="mean_dur "
      Temp$participation = gsub(".*(_)(.*).csv", "\\2", list_par[i],".csv")
      Temp$n = nrow(fichier_i_nuit_sp)
      Temp$esp = Species
      Table_Sp = rbind(Table_Sp, Temp)
    }
    Table_Night = rbind(Table_Night, Table_Sp)
  }
  Table_tot = rbind(Table_tot, Table_Night)
  write.csv(Table_tot, paste0(arg[2], "duree_moy_", gsub(".*(_)(.*).csv", "\\2", list_par[i],".csv"), ".csv"), row.names = F)
}



    

