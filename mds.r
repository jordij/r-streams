library(dplyr)
library(extrafont)
library(ggplot2)
library(cowplot)
library(vegan)

#############
## WARNING ##
#############
## I like using Gill Sans Nova typeface for plots and maps.
## If you need to load your fonts in Windows, uncomment the line below.
## If you don't have Gill Sans Nova locally just replace all occurences of `pfamily="Gill Sans Nove"`
## or with your typeface of choice.

# loadfonts(device="win")

pfamily <- "Gill Sans Nova"
ptextsize <- 18

# Load and polish data
dsamples <- read.table(file="./data/auckland_streams.csv", sep=",", header=TRUE)
dsites <- read.table(file="./data/sites.csv", sep=",", header=TRUE)
# Polish date
dsamples$date <- as.Date(dsamples$date, format="%d/%m/%Y")
# Keep only sites we want
dsamples <- dsamples[dsamples$site %in% dsites$sitename,]
dsamples <- droplevels(dsamples)

##############################################################
# Scatter plots with regression lines
# By Native Forest
##############################################################

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

# Subset for stream, sitetype and taxas only
dtsamples <- subset(dsamples, select = -c(year, date, sitenumber, site, protocol, richness, EPT, MCI, SQMCI, NativeForest, Pasture, Forestry, Urban, Horticulture))
# replace abundance codes by values R=1, C=5, A=20, VA=100, VVA=500
abundanceToInt <- function(a) {
    case_when(
        a == "R" ~ 1,
        a == "C" ~ 5,
        a == "A" ~ 20,
        a == "VA" ~ 100,
        a == "VVA" ~ 500,
        a == ""  ~ 0
    )
}

####################
# Plot MDS stuff
####################

diffcols <- setdiff(colnames(dtsamples), list("streamname", "sitetype"))
dtsamples[diffcols] <- lapply(dtsamples[diffcols], abundanceToInt)

not_all_na <- function(x) any(!is.na(x))
dtsamples <- dtsamples %>% select_if(not_all_na)
# update as some got removed
diffcols <- setdiff(colnames(dtsamples), list("streamname", "sitetype"))
# save streamnames-types separately
dtypes <- dtsamples %>% distinct(sitetype, streamname, .keep_all = F)
dtsamples$sitetype <- NULL # drop type, will add later
dtsamples_simpl <- aggregate(. ~ streamname, transform(dtsamples, streamname = streamname), mean)

dtsamples_simpl$sitetype <- dtypes$sitetype

dtsamples_co <- metaMDS(comm = dtsamples_simpl[diffcols], distance = "bray", trace = FALSE, autotransform = FALSE)
spsc <- as.data.frame(scores(dtsamples_co, "species"))
# add species and type back
spsc$species <- rownames(spsc)

MDS_xy <- data.frame(dtsamples_co$points)
MDS_xy$streamname <- dtsamples_simpl$streamname
MDS_xy$sitetype <- dtsamples_simpl$sitetype

mdsplot <- ggplot(MDS_xy, aes(MDS1, MDS2)) + 
    geom_text(aes(label=streamname, colour=sitetype), size=5) + # stream names
    geom_point(data=spsc, aes(x=NMDS1,y=NMDS2), size=1, alpha=0.5) +  # add the species labels
    theme(plot.title=element_text(hjust=0.5),
        legend.text=element_text(size=ptextsize-4),
        axis.title.x=element_text(size=ptextsize-2, family=pfamily),
        axis.title.y=element_text(size=ptextsize-2, family=pfamily),
        axis.text=element_text(size=ptextsize-8 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5)) +
    scale_colour_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    guides(colour=guide_legend(title=""))
    
png(file="./output/mds.png", width=800, height=400)
print(mdsplot)
dev.off()