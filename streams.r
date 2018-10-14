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


###################
# Indices vs forest
###################

##############################################################
# Scatter plots with regression lines
# By Native Forest
##############################################################

mci_plot <- ggplot(dsamples, aes(x=NativeForest, y=MCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 119, color = "green", size = 1) +
    geom_hline(yintercept = 100, color = "blue", size = 1) +
    geom_hline(yintercept = 80, color = "red", size = 1) +
    labs(title="", x="Catchment in native forest (%)", y="MCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 140, 20), limits=c(0,140)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

sqmci_plot <- ggplot(dsamples, aes(x=NativeForest, y=SQMCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 5.99, color = "green", size = 1) +
    geom_hline(yintercept = 5, color = "blue", size = 1) +
    geom_hline(yintercept = 4, color = "red", size = 1) +
    labs(title="", x="Catchment in native forest (%)", y="SQMCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 10, 2), limits=c(0,10)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

ept_plot <- ggplot(dsamples, aes(x=NativeForest, y=EPT)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in native forest (%)", y="%EPT", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

tr_plot <- ggplot(dsamples, aes(x=NativeForest, y=richness)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in native forest (%)", y="Taxa richness", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

pgrid <- plot_grid(mci_plot, sqmci_plot, ept_plot, tr_plot, labels=c("(a)", "(b)", "(c)", "(d)"))
png(file="./output/indices_vs_nforest.png", width=800, height=800)
print(pgrid)
dev.off()


##############################################################
# Scatter plots with regression lines
# By Urban Land Use
##############################################################

mci_plot <- ggplot(dsamples, aes(x=Urban, y=MCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 119, color = "green", size = 1) +
    geom_hline(yintercept = 100, color = "blue", size = 1) +
    geom_hline(yintercept = 80, color = "red", size = 1) +
    labs(title="", x="Catchment in urban land use (%)", y="MCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 140, 20), limits=c(0,140)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

sqmci_plot <- ggplot(dsamples, aes(x=Urban, y=SQMCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 5.99, color = "green", size = 1) +
    geom_hline(yintercept = 5, color = "blue", size = 1) +
    geom_hline(yintercept = 4, color = "red", size = 1) +
    labs(title="", x="Catchment in urban land use (%)", y="SQMCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 10, 2), limits=c(0,10)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

ept_plot <- ggplot(dsamples, aes(x=Urban, y=EPT)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in urban land use (%)", y="%EPT", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

tr_plot <- ggplot(dsamples, aes(x=Urban, y=richness)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in urban land use (%)", y="Taxa richness", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

pgrid <- plot_grid(mci_plot, sqmci_plot, ept_plot, tr_plot, labels=c("(a)", "(b)", "(c)", "(d)"))
png(file="./output/indices_vs_urban.png", width=800, height=800)
print(pgrid)
dev.off()

##############################################################
# Scatter plots with regression lines
# By Pasture Land Use
##############################################################

mci_plot <- ggplot(dsamples, aes(x=Pasture, y=MCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 119, color = "green", size = 1) +
    geom_hline(yintercept = 100, color = "blue", size = 1) +
    geom_hline(yintercept = 80, color = "red", size = 1) +
    labs(title="", x="Catchment in pasture land use (%)", y="MCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 140, 20), limits=c(0,140)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

sqmci_plot <- ggplot(dsamples, aes(x=Pasture, y=SQMCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 5.99, color = "green", size = 1) +
    geom_hline(yintercept = 5, color = "blue", size = 1) +
    geom_hline(yintercept = 4, color = "red", size = 1) +
    labs(title="", x="Catchment in pasture land use (%)", y="SQMCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 10, 2), limits=c(0,10)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

ept_plot <- ggplot(dsamples, aes(x=Pasture, y=EPT)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in pasture land use (%)", y="%EPT", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

tr_plot <- ggplot(dsamples, aes(x=Pasture, y=richness)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in pasture land use (%)", y="Taxa richness", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

pgrid <- plot_grid(mci_plot, sqmci_plot, ept_plot, tr_plot, labels=c("(a)", "(b)", "(c)", "(d)"))
png(file="./output/indices_vs_pasture.png", width=800, height=800)
print(pgrid)
dev.off()

# Scatter plots with regression lines
# By Forestry land use
mci_plot <- ggplot(dsamples, aes(x=Forestry, y=MCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    geom_hline(yintercept = 119, color = "green", size = 1) +
    geom_hline(yintercept = 100, color = "blue", size = 1) +
    geom_hline(yintercept = 80, color = "red", size = 1) +
    labs(title="", x="Catchment in forestry (%)", y="MCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 140, 20), limits=c(0,140)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

sqmci_plot <- ggplot(dsamples, aes(x=Forestry, y=SQMCI)) + 
    geom_point() +
    geom_smooth(method = lm, se = FALSE, color="black") +
    geom_hline(yintercept = 5.99, color = "green", size = 1) +
    geom_hline(yintercept = 5, color = "blue", size = 1) +
    geom_hline(yintercept = 4, color = "red", size = 1) +
    labs(title="", x="Catchment in forestry land use (%)", y="SQMCI", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 10, 2), limits=c(0,10)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

ept_plot <- ggplot(dsamples, aes(x=Forestry, y=EPT)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    # geom_smooth(method = loess, se = FALSE, color="grey") +
    labs(title="", x="Catchment in forestry land use (%)", y="%EPT", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

tr_plot <- ggplot(dsamples, aes(x=Forestry, y=richness)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="Catchment in forestry land use (%)", y="Taxa richness", color="") +
    scale_x_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) +
    scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

pgrid <- plot_grid(mci_plot, sqmci_plot, ept_plot, tr_plot, labels=c("(a)", "(b)", "(c)", "(d)"))
png(file="./output/indices_vs_forestry.png", width=800, height=800)
print(pgrid)
dev.off()

###################
# SQMCI vs MCI
###################

mci_plot <- ggplot(dsamples, aes(x=SQMCI, y=MCI)) + 
    geom_point() +
    geom_smooth(method = lm, color="black") +
    labs(title="", x="SQMCI", y="MCI", color="") +
    scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize, family=pfamily),
        rect=element_blank()) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

png(file="./output/mci_vs_sqmci.png", width=800, height=800)
print(mci_plot)
dev.off()


##############################################################
# Mean bars for MCI, SQMCI, taxa richness and EPT by site type
##############################################################

means <- dsamples %>% group_by(sitetype) %>% 
   summarise(mean = mean(MCI), sd=sd(MCI))
mci_bars <- ggplot(means, aes(sitetype, mean, fill=sitetype)) +             
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    geom_hline(yintercept = 119, color = "green", size = 1) +
    geom_hline(yintercept = 100, color = "blue", size = 1) +
    geom_hline(yintercept = 80, color = "red", size = 1) +
    labs(x = "Site type", y = "MCI") +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-2, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

means <- dsamples %>% group_by(sitetype) %>% 
   summarise(mean = mean(SQMCI), sd=sd(SQMCI))
sqmci_bars <- ggplot(means, aes(sitetype, mean, fill=sitetype)) +             
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    geom_hline(yintercept = 5.99, color = "green", size = 1) +
    geom_hline(yintercept = 5, color = "blue", size = 1) +
    geom_hline(yintercept = 4, color = "red", size = 1) +
    labs(x = "Site type", y = "SQMCI") +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-2, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

means <- dsamples %>% group_by(sitetype) %>% 
   summarise(mean = mean(EPT), sd=sd(EPT))
ept_bars <- ggplot(means, aes(sitetype, mean, fill=sitetype)) +             
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    labs(x = "Site type", y = "%EPT") +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-2, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

means <- dsamples %>% group_by(sitetype) %>% 
   summarise(mean = mean(richness), sd=sd(richness))
tr_bars <- ggplot(means, aes(sitetype, mean, fill=sitetype)) +             
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    scale_y_continuous(breaks=c(0, 10, 20, 30, 40)) +
    labs(x = "Site type", y = "Taxa richness") +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-2, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))


bars_plot <- plot_grid(sqmci_bars, mci_bars, ept_bars, tr_bars, labels=c("(a)", "(b)", "(c)", "(d)"))

png(file="./output/bars_plot.png", width=800, height=600)
print(bars_plot)
dev.off()

########################################################################
# Bar plots for all sites using all indices SQMCI, MCI, richness and EPT
########################################################################

means <- dsamples %>% group_by(streamname) %>% 
   summarise(mean = mean(MCI), sd=sd(MCI), sitetype=unique(sitetype))
# order decreasing by mean
means$streamname <- factor(means$streamname, levels = means$streamname[order(means$mean, decreasing=TRUE)])

mci_bars <- ggplot(means, aes(streamname, mean, fill=sitetype)) +             
    geom_bar(stat = "identity", width=c(0.5)) +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    geom_hline(yintercept = 119, color = "green", size = 0.5) +
    geom_hline(yintercept = 100, color = "blue", size = 0.5) +
    geom_hline(yintercept = 80, color = "red", size = 0.5) +
    labs(x = "Site type", y = "MCI") +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-4, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-7 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

means <- dsamples %>% group_by(streamname) %>% 
   summarise(mean = mean(SQMCI), sd=sd(SQMCI), sitetype=unique(sitetype))
# order decreasing by mean
means$streamname <- factor(means$streamname, levels = means$streamname[order(means$mean, decreasing=TRUE)])
sqmci_bars <- ggplot(means, aes(streamname, mean, fill=sitetype)) +             
    geom_bar(stat = "identity", width=c(0.5)) +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    geom_hline(yintercept = 5.99, color = "green", size = 0.5) +
    geom_hline(yintercept = 5, color = "blue", size = 0.5) +
    geom_hline(yintercept = 4, color = "red", size = 0.5) +
    labs(x = "Site type", y = "SQMCI") +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-4, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-7 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

means <- dsamples %>% group_by(streamname) %>% 
   summarise(mean = mean(EPT), sd=sd(EPT), sitetype=unique(sitetype))
# order decreasing by mean
means$streamname <- factor(means$streamname, levels = means$streamname[order(means$mean, decreasing=TRUE)])
ept_bars <- ggplot(means, aes(streamname, mean, fill=sitetype)) +             
    geom_bar(stat = "identity", width=c(0.5)) +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    labs(x = "Site type", y = "%EPT") +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-4, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize-7 , family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

means <- dsamples %>% group_by(streamname) %>% 
   summarise(mean = mean(richness), sd=sd(richness), sitetype=unique(sitetype))
# order decreasing by mean
means$streamname <- factor(means$streamname, levels = means$streamname[order(means$mean, decreasing=TRUE)])
tr_bars <- ggplot(means, aes(streamname, mean, fill=sitetype)) +             
    geom_bar(stat = "identity", width=c(0.5)) +
    geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
    scale_y_continuous(breaks=c(0, 10, 20, 30, 40)) +
    labs(x = "Site type", y = "Taxa richness") +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.y=element_text(size=ptextsize-4, family=pfamily),
        axis.title.x=element_blank(),
        axis.text=element_text(size=ptextsize- 7, family=pfamily, colour = "black"),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=ptextsize-2, family=pfamily),
        rect=element_blank()) +
    guides(fill=FALSE) +
    scale_fill_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
    theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

bars_plot <- plot_grid(sqmci_bars, mci_bars, ept_bars, tr_bars, labels=c("(a)", "(b)", "(c)", "(d)"), ncol=1)
png(file="./output/bars_plot_by_site.png", width=800, height=600)
print(bars_plot)
dev.off()


# #################################
# # Time series plots for ALL sites
# #################################


sites_series = levels(dsamples$site)

for(site in sites_series) {

    dtsub <- dsamples[dsamples$site == site, ]

    sqmci_plot <- ggplot(dtsub, aes(x=year, y=SQMCI)) + 
        geom_point() +
        geom_smooth(method = loess, color="black") +
        geom_hline(yintercept = 5.99, color = "green", size = 1) +
        geom_hline(yintercept = 5, color = "blue", size = 1) +
        geom_hline(yintercept = 4, color = "red", size = 1) +
        labs(title="", x="Year", y="SQMCI", color="") +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        scale_y_continuous(breaks=seq(0, 10, 2), limits=c(0,10)) +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(size=ptextsize, family=pfamily),
            axis.title.y=element_text(size=ptextsize, family=pfamily),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize, family=pfamily),
            rect=element_blank()) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

    mci_plot <- ggplot(dtsub, aes(x=year, y=MCI)) + 
        geom_point() +
        geom_smooth(method = loess, color="black", fullrange=TRUE) +
        geom_hline(yintercept = 119, color = "green", size = 0.5) +
        geom_hline(yintercept = 100, color = "blue", size = 0.5) +
        geom_hline(yintercept = 80, color = "red", size = 0.5) +
        labs(title="", x="Year", y="MCI", color="") +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        scale_y_continuous(breaks=seq(0, 140, 20), limits=c(-10,160)) +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(size=ptextsize, family=pfamily),
            axis.title.y=element_text(size=ptextsize, family=pfamily),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize, family=pfamily),
            rect=element_blank()) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

    ept_plot <- ggplot(dtsub, aes(x=year, y=EPT)) + 
        geom_point() +
        geom_smooth(method = loess, color="black", fullrange=TRUE) +
        labs(title="", x="Year", y="%EPT", color="") +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        scale_y_continuous(breaks=seq(0, 50, 10), limits=c(-10,60)) +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(size=ptextsize, family=pfamily),
            axis.title.y=element_text(size=ptextsize, family=pfamily),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize, family=pfamily),
            rect=element_blank()) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

    tr_plot <- ggplot(dtsub, aes(x=year, y=richness)) + 
        geom_point() +
        geom_smooth(method = loess, color="black") +
        labs(title="", x="Year", y="Taxa richness", color="") +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        scale_y_continuous(breaks=seq(0, 50, 10), limits=c(0,50)) +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(size=ptextsize, family=pfamily),
            axis.title.y=element_text(size=ptextsize, family=pfamily),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize, family=pfamily),
            rect=element_blank()) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))
    
    bars_plot <- plot_grid(mci_plot, sqmci_plot, ept_plot, tr_plot, labels=c("(a)", "(b)", "(c)", "(d)"), ncol=2)
    png(file=paste("./output/indices_plots_", site, "_.png", sep=""), width=1200, height=600)
    print(bars_plot)
    dev.off()
}


sites_series = unique(dsamples$sitetype)

for(site in sites_series) {
    dtsub <- dsamples[dsamples$sitetype == site, ]
    means <- dtsub %>% group_by(year) %>% 
        summarise(mean = mean(MCI), sd=sd(MCI))
    mci_bars <- ggplot(means, aes(year, mean)) +             
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
        geom_hline(yintercept = 119, color = "green", size = 1) +
        geom_hline(yintercept = 100, color = "blue", size = 1) +
        geom_hline(yintercept = 80, color = "red", size = 1) +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        labs(x = "Year", y = "MCI") +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.y=element_text(size=ptextsize-2, family=pfamily),
            axis.title.x=element_blank(),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.ticks.x=element_blank(),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize-2, family=pfamily),
            rect=element_blank()) +
        guides(fill=FALSE) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

    means <- dtsub %>% group_by(year) %>% 
        summarise(mean = mean(SQMCI), sd=sd(SQMCI))
    sqmci_bars <- ggplot(means, aes(year, mean)) +             
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
        geom_hline(yintercept = 5.99, color = "green", size = 1) +
        geom_hline(yintercept = 5, color = "blue", size = 1) +
        geom_hline(yintercept = 4, color = "red", size = 1) +
        labs(x = "Year", y = "SQMCI") +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.y=element_text(size=ptextsize-2, family=pfamily),
            axis.title.x=element_blank(),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.ticks.x=element_blank(),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize-2, family=pfamily),
            rect=element_blank()) +
        guides(fill=FALSE) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

    means <- dtsub %>% group_by(year) %>% 
        summarise(mean = mean(EPT), sd=sd(EPT))
    ept_bars <- ggplot(means, aes(year, mean)) +             
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        labs(x = "Year", y = "%EPT") +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.y=element_text(size=ptextsize-2, family=pfamily),
            axis.title.x=element_blank(),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.ticks.x=element_blank(),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize-2, family=pfamily),
            rect=element_blank()) +
        guides(fill=FALSE) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))

    means <- dtsub %>% group_by(year) %>% 
        summarise(mean = mean(richness), sd=sd(richness))
    tr_bars <- ggplot(means, aes(year, mean)) +             
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymax =  mean +  sd, ymin = mean), width = 0.25) +
        scale_x_continuous(breaks=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        labs(x = "Year", y = "Taxa richness") +
        theme(plot.title=element_text(hjust=0.5),
            axis.title.y=element_text(size=ptextsize-2, family=pfamily),
            axis.title.x=element_blank(),
            axis.text=element_text(size=ptextsize-2 , family=pfamily, colour = "black"),
            axis.ticks.x=element_blank(),
            axis.line = element_line(colour = "black"),
            text=element_text(size=ptextsize-2, family=pfamily),
            rect=element_blank()) +
        guides(fill=FALSE) +
        theme(panel.grid=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=0.5))


    bars_plot <- plot_grid(mci_bars, sqmci_bars, ept_bars, tr_bars, labels=c("(a)", "(b)", "(c)", "(d)"), ncol=2)
    png(file=paste("./output/bars_plot_type_year_", site, "_.png", sep=""), width=1200, height=600)
    print(bars_plot)
    dev.off()
}