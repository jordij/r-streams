library(measurements)
library(southernMaps)
library(extrafont)

#############
## WARNING ##
#############
## I like using Gill Sans Nova typeface for plots and maps.
## If you need to load your fonts in Windows, uncomment the line below.
## If you don't have Gill Sans Nova locally just replace all occurences of `pfamily="Gill Sans Nove"`
## or with your typeface of choice.

# loadfonts(device="win")

#############
# Sites map
#############

pfamily <- "Gill Sans Nova"
ptextsize <- 18

site_coords <- read.table(file="./data/sites_coords.csv", sep=",", header=TRUE)
site_coords$newlon <- as.numeric(measurements::conv_unit(site_coords$lon, from = 'deg_min_sec', to = 'dec_deg'))
site_coords$newlat <- as.numeric(measurements::conv_unit(site_coords$lat, from = 'deg_min_sec', to = 'dec_deg'))

nz_DF <- fortify_polygons(shape = nzHigh84, "wgs84")

plot <- ggplot(nz_DF, aes(x = long, y = lat)) + 
            geom_polygon(aes(group = group), color="#2b2b2b", fill="grey", alpha=0.5) +
            geom_text(data=site_coords, aes(x = newlon, y = newlat, colour = type, label=num), family=pfamily, size=8) +
            coord_map(xlim = c(174, 175.4),ylim = c(-37.4, -36.2)) + 
            labs(x = "Long. E", y = "Lat. S", title="") +
            scale_colour_manual(values=c("#006266", "#009900", "#FF6633", "#993333")) +
            theme(plot.title=element_text(hjust=0.5),
                legend.text=element_text(size=ptextsize),
                legend.title=element_blank(),
                axis.title.x=element_text(size=ptextsize, family=pfamily),
                axis.title.y=element_text(size=ptextsize, family=pfamily),
                axis.text=element_text(size=ptextsize-4 , family=pfamily, colour = "black"),
                axis.line = element_line(colour = "black"),
                text=element_text(size=ptextsize, family=pfamily),
                rect=element_blank(),
                panel.grid=element_blank(), 
                panel.border=element_rect(colour = "black", fill=NA, size=0.5))

png(file="./output/sites_map.png", width=800, height=800)
print(plot)
dev.off()