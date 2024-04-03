#
# Aggregation of all tables
# 

library(data.table)
library(tidyverse)

# Read list of recording sessions
particip <- fread("./example files/data_sel.csv") # metadata of recording sessions

# List files
var <- list.files(path = "./example files/bat_pass_duration/standard_deviation/", full.names = T) # standard deviation table
dur <- list.files(path = "./example files/bat_pass_duration/mean_bat_pass_duration/", full.names = T) # bat pass duration table
buzz <- list.files(path = "./example files/nb_buzz_per_night", full.names = T) # number of buzzes per species and per night table
contact <- list.files(path = "./example files/nb_pat_passes_per_night/", full.names = T) # number of bat passes and sum of bat passes duration per night table

# standard deviation
Table_var <- data.frame()

for (i in 1:length(var)) {
  read_var <- fread(var[i])
  read_var <- as.data.frame(read_var)
  read_var$participation <- substr(basename(var[i]), 9, nchar(basename(var[i]))-4)
  Table_var <- rbind(Table_var, read_var)
}

Table_var <- subset(Table_var, Table_var$n>=20) #2913

# Bat pass duration
Table_dur <- data.frame()

for (j in 1:length(dur)) {
  read_dur <- fread(dur[j])
  read_dur <- as.data.frame(read_dur)
  read_dur$participation <- substr(basename(dur[j]), 11, nchar(basename(dur[j]))-4)
  Table_dur <- rbind(Table_dur, read_dur)
}

Table_dur <- subset(Table_dur, Table_dur$n>=20) #2902

# number of buzzes per species and per night
Table_buzz <- data.frame()

for (k in 1:length(buzz)) {
  read_buzz <- fread(buzz[k])
  read_buzz <- as.data.frame(read_buzz)
  Table_buzz <- rbind(Table_buzz, read_buzz)
}

# number of bat passes and sum of bat passes duration per night
Table_contact <- data.frame()

for (l in 1:length(contact)) {
  read_contact <- fread(contact[l])
  read_contact <- as.data.frame(read_contact)
  Table_contact <- rbind(Table_contact, read_contact)
}

Table_contact <- subset(Table_contact, Table_contact$nb_contact>=20) #2913


# Join tables
Table_dur$n <- NULL
Table_var$n <- NULL
Table_var$nuit=as.Date(Table_var$nuit)
Table_dur$nuit=as.Date(Table_dur$nuit)
Table_buzz$nuit=as.Date(Table_buzz$nuit)
Table_contact$nuit=as.Date(Table_contact$nuit)
Table_buzz$participation=as.character(Table_buzz$participation)
Table_contact$participation=as.character(Table_contact$participation)

data <- left_join(Table_var, Table_dur)
data <- left_join(data, Table_buzz)
data_final <- left_join(data, Table_contact)

# replace NA by 0 in nb of buzzes
data_final$nb_buzz[is.na(data_final$nb_buzz)] <- 0
sum(is.na(data_final$nb_buzz))


# Merge with metadata
data_part_sel <- merge(data_final, particip, all.x = T)

# Save
write.csv(data_part_sel, "./example files/final_data_update.csv", row.names = F)
