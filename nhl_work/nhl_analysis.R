# library(readxl)
library(data.table)
library(chron)

# skater_stats <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 1)
# player_list <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 2)
# team_list <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 3)
# pos_list <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 4)
skater_stats <- fread("nhl_data/skater_stats.csv")

summary(skater_stats)

# Delete col "V1"
skater_stats$V1 <- NULL

# Rename columnes: "+/-", "S%", "FO%"
colnames(skater_stats)[colnames(skater_stats) == "+/-"] <- "PlusMinus"
colnames(skater_stats)[colnames(skater_stats) == "S%"] <- "S_perc"
colnames(skater_stats)[colnames(skater_stats) == "FO%"] <- "FO_perc"

# Delete data before 2008, because some data was not collected before
skater_stats <- skater_stats[Season >= 2008]

# Converting to numeric (or 'times') where neccesary
skater_stats[, G := as.numeric(G)]
skater_stats[, A := as.numeric(A)]
skater_stats[, PTS := as.numeric(PTS)]
skater_stats[, PlusMinus := as.numeric(PlusMinus)]
skater_stats[, PIM := as.numeric(PIM)]
skater_stats[, EVG := as.numeric(EVG)]
skater_stats[, PPG := as.numeric(PPG)]
skater_stats[, SHG := as.numeric(SHG)]
skater_stats[, GWG := as.numeric(GWG)]
skater_stats[, EVA := as.numeric(EVA)]
skater_stats[, PPA := as.numeric(PPA)]
skater_stats[, SHA := as.numeric(SHA)]
skater_stats[, S := as.numeric(S)]
skater_stats[, S_perc := as.numeric(S_perc)]
skater_stats$ATOI = paste0("00:", skater_stats$ATOI)     # needs to be converted to hours first
skater_stats[, ATOI := times(skater_stats$ATOI)]         # special numeric type for time
skater_stats$TOI = skater_stats$ATOI*skater_stats$GP     # setting TOI from ATOI and GP to have time format

# skater_stats$TOI = gsub(",", "", skater_stats$TOI) delete ","-s 
# skater_stats[, TOI := as.numeric(TOI)]                 in case time format is not needed


# Changing NA to 0 where that makes sense (basically everywhere except FO_perc)
skater_stats$G[is.na(skater_stats$G)] <- 0
skater_stats$A[is.na(skater_stats$A)] <- 0
skater_stats$PTS[is.na(skater_stats$PTS)] <- 0
skater_stats$PlusMinus[is.na(skater_stats$PlusMinus)] <- 0
skater_stats$PIM[is.na(skater_stats$PIM)] <- 0
skater_stats$EVG[is.na(skater_stats$EVG)] <- 0
skater_stats$PPG[is.na(skater_stats$PPG)] <- 0
skater_stats$SHG[is.na(skater_stats$SHG)] <- 0
skater_stats$GWG[is.na(skater_stats$GWG)] <- 0
skater_stats$EVA[is.na(skater_stats$EVA)] <- 0
skater_stats$PPA[is.na(skater_stats$PPA)] <- 0
skater_stats$SHA[is.na(skater_stats$SHA)] <- 0
skater_stats$S[is.na(skater_stats$S)] <- 0
skater_stats$S_perc[is.na(skater_stats$S_perc)] <- 0
skater_stats$BLK[is.na(skater_stats$BLK)] <- 0
skater_stats$HIT[is.na(skater_stats$HIT)] <- 0
skater_stats$FOwin[is.na(skater_stats$FOwin)] <- 0
skater_stats$FOloss[is.na(skater_stats$FOloss)] <- 0

summary(skater_stats)