# merging trees, seedlings and saplings
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)
seedling <- read.csv("seedling.csv", stringsAsFactors = F)
all <- rbind(tree, sapling); rm(tree,sapling) 
colnames(all)
colnames(seedling)

# adding type to seedling
seedling$TYPE <- "SEEDLING"
# remove quad
seedling <- seedling[-7]
# remove subplot counts
seedling <- seedling[-8] ; seedling <- seedling[-8]
# add seedling COUNT_HA
seedling$COUNT_HA <- seedling$DECID_COUNT_HA + seedling$CONIF_COUNT_HA
# remove basal area from saplings/trees
all <- all[-10]
all <- all[-8]
all <- all[-23] # x 2
# adding count per meter in all 
all$DECID_COUNT_M <- all$DECID_COUNT_HA / 10000
all$CONIF_COUNT_M <- all$CONIF_COUNT_HA / 10000
colnames(all)
colnames(seedling)
all <- rbind(all, seedling) ; rm(seedling)
write.csv(all,"all.csv", row.names = F)

# changing upland / lowland (for now)
all <- read.csv("all.csv")
all$SITE[all$SITE == "Upland"] <- "DALTON"
all$SITE[all$SITE == "Lowland"] <- "STEESE"
write.csv(all, "all.csv", row.names = F)