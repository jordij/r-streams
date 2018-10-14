


# Load and polish data
dsamples <- read.table(file="./data/auckland_streams.csv", sep=",", header=TRUE)
dsites <- read.table(file="./data/sites.csv", sep=",", header=TRUE)
# Polish date
dsamples$date <- as.Date(dsamples$date, format="%d/%m/%Y")
# Keep only sites we want
dsamples <- dsamples[dsamples$site %in% dsites$sitename,]
dsamples <- droplevels(dsamples)

##############################################################
# Generate stats summary for EPT, Taxa richness, MCI and SQMCI
##############################################################

dsumms <- summarise_at(group_by(dsamples, site), .vars=c("richness", "EPT", "MCI", "SQMCI"), funs(n(), mean, min, max, sd))
print(dsumms)

dsamples <- merge(dsamples, dsites, by.x = c("site"), by.y = c("sitename"))
# Change some levels otherwise labels overlap
levels(dsamples$streamname)[levels(dsamples$streamname) == "Otara"] <- "Otara"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Mahurangi"] <- "Mahurangi"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Oakley LTB"] <- "Oakley"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Ngakaroa"] <- "Ngakaroa"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Puhinui"] <- "Puhinui"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Papakura LTB"] <- "Papakura"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Oteha"] <- "Oteha"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Kumeu"] <- "Kumeu"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Hoteo"] <- "Hoteo"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Vaughns Lower"] <- "Vaughn"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Wairoa"] <- "Wairoa"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Matakana]"] <- "Matakana"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Opanuku"] <- "Opanuku"
levels(dsamples$streamname)[levels(dsamples$streamname) == "West Hoe"] <- "West Hoe"
levels(dsamples$streamname)[levels(dsamples$streamname) == "Cascade LTB"] <- "Cascades"

# Pearson summary

cor.test(dsamples$NativeForest, dsamples$MCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$NativeForest, dsamples$SQMCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$NativeForest, dsamples$EPT, method = "pearson", conf.level = 0.95)
cor.test(dsamples$NativeForest, dsamples$richness, method = "pearson", conf.level = 0.95)

# Linear regression summary

lmMod <- lm(NativeForest ~ MCI, data=dsamples)
summary (lmMod)
lmMod <- lm(NativeForest ~ SQMCI, data=dsamples)
summary (lmMod)
lmMod <- lm(NativeForest ~ EPT, data=dsamples)
summary (lmMod)
lmMod <- lm(NativeForest ~ richness, data=dsamples)
summary (lmMod)

# Pearson summary

cor.test(dsamples$Urban, dsamples$MCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Urban, dsamples$SQMCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Urban, dsamples$EPT, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Urban, dsamples$richness, method = "pearson", conf.level = 0.95)

# Linear regression summary

lmMod <- lm(Urban ~ MCI, data=dsamples)
summary (lmMod)
lmMod <- lm(Urban ~ SQMCI, data=dsamples)
summary (lmMod)
lmMod <- lm(Urban ~ EPT, data=dsamples)
summary (lmMod)
lmMod <- lm(Urban ~ richness, data=dsamples)
summary (lmMod)

# Pearson summary

cor.test(dsamples$Pasture, dsamples$MCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Pasture, dsamples$SQMCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Pasture, dsamples$EPT, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Pasture, dsamples$richness, method = "pearson", conf.level = 0.95)

# Linear regression summary

lmMod <- lm(Pasture ~ MCI, data=dsamples)
summary (lmMod)
lmMod <- lm(Pasture ~ SQMCI, data=dsamples)
summary (lmMod)
lmMod <- lm(Pasture ~ EPT, data=dsamples)
summary (lmMod)
lmMod <- lm(Pasture ~ richness, data=dsamples)
summary (lmMod)

# Pearson summary

cor.test(dsamples$Forestry, dsamples$MCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Forestry, dsamples$SQMCI, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Forestry, dsamples$EPT, method = "pearson", conf.level = 0.95)
cor.test(dsamples$Forestry, dsamples$richness, method = "pearson", conf.level = 0.95)

# Linear regression summary

lmMod <- lm(Forestry ~ MCI, data=dsamples)
summary (lmMod)
lmMod <- lm(Forestry ~ SQMCI, data=dsamples)
summary (lmMod)
lmMod <- lm(Forestry ~ EPT, data=dsamples)
summary (lmMod)
lmMod <- lm(Forestry ~ richness, data=dsamples)
summary (lmMod)

# MCI vs SQMCI Pearson and regression
cor.test(dsamples$SQMCI, dsamples$MCI, method = "pearson", conf.level = 0.95)
lmMod <- lm(SQMCI ~ MCI, data=dsamples)
summary (lmMod)

# Sites analysis

sites_series = levels(dsamples$site)

for(site in sites_series) {
    dtsub <- dsamples[dsamples$site == site, ]
    print(site)
    print(cor.test(dtsub$year, dtsub$MCI,  method = "kendall", conf.level = 0.95))
    print(cor.test(dtsub$year, dtsub$EPT,  method = "kendall", conf.level = 0.95))
}

print(cor.test(dsamples$year, dsamples$MCI,  method = "spearman", conf.level = 0.95))
print(cor.test(dsamples$year, dsamples$EPT,  method = "spearman", conf.level = 0.95))

sites_series = unique(dsamples$sitetype)

for(site in sites_series) {
    dtsub <- dsamples[dsamples$sitetype == site, ]
    print(site)
    print(cor.test(dtsub$year, dtsub$MCI,  method = "spearman", conf.level = 0.95))
    print(cor.test(dtsub$year, dtsub$EPT,  method = "spearman", conf.level = 0.95))
    # by mean
    means <- dtsub %>% group_by(year) %>% 
        summarise(mean = mean(MCI), sd=sd(MCI))
    print(cor.test(means$year, means$mean,  method = "spearman", conf.level = 0.95))
    means <- dtsub %>% group_by(year) %>% 
        summarise(mean = mean(EPT), sd=sd(EPT))
    print(cor.test(means$year, means$mean,  method = "spearman", conf.level = 0.95))
}