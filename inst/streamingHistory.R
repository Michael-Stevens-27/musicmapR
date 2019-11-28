#-------------------------------------------------------------------------------
# packages for data manipulation and plotting
library(viridis)
library(rjson)
library(ggplot2)
library(rvest)
library(stringr)
library(raster)
library(leaflet)

#-------------------------------------------------------------------------------
devtools::source_url("https://github.com/Michael-Stevens-27/musicmapR/blob/master/SpotifyFunctions.R")

#-------------------------------------------------------------------------------
# read file and process
data  <- initProcess("/home/mstevens/Desktop/Job\ Stuff/BLOG/001-MichaelStevens/StreamingHistory.json")

# concatonate the artists to gather an idea of play count per artist  
namesTable <- table(data$artistName) 

# plot as a barplot
bins <- 15
png(file = "top15.png", width = 2000, height = 1000, pointsize = 20)
barplot(head(rev(namesTable[order(namesTable)]), bins), xlab = NULL, 
        ylab = "plays (3 months)", col = plasma(bins), width = 0.8, las = 2,
        cex.names = 0.5)
dev.off()

# plot time series in polar co ord form 

timeSeriesPlot <- plotTimeSeries(data)
ggsave(file = "joeTimeSeries.png", plot = timeSeriesPlot, units = "cm", height = 7, width = 7, dpi = 600)

attach(data)
#-------------------------------------------------------------------------------
# harvest band locations using rvest
uniqueArtists <- unique(artistName)

#----------------------------------------------------------------
# load the scrape data
# load("/home/mstevens/Desktop/BLOG/001-MichaelStevens/WikiNames.Rdata")
# load("/home/mstevens/Desktop/BLOG/001-MichaelStevens/lastfmNames.Rdata")

# scrape locations for artists from lastFM
# joeMessy <- scrapeLocations(uniqueArtists)
# save(joeMessy, file = "messylastFM.Rdata")

# call in a shapefile with countries listed
countriesShape <- readOGR("/home/mstevens/Desktop/Job\ Stuff/BLOG/001-MichaelStevens/countriesSHP")
load("/home/mstevens/Desktop/Job\ Stuff/BLOG/001-MichaelStevens/lastfmNames.Rdata")

# use some basic regular expression function to check if each entry is in the 
# list of countries
countryCount <- rep(0, length(countriesShape$NAME_LONG))
countryOrigin <- rep(NA, length(uniqueArtists))
artistVec <- rep(NA, length(countriesShape$NAME_LONG))

for(i in 1:length(countriesShape$NAME_LONG))
{
  perCount <- str_extract(originsLastFM$origin.2, fixed(countriesShape$NAME_LONG[i], ignore_case = TRUE))
  countryCount[i] <- sum(!is.na(perCount))
  artistVec[i] <- paste(as.character(uniqueArtists[!is.na(perCount)]), collapse = ', ')
  countryOrigin[!is.na(perCount)] <- countriesShape$NAME_LONG[i]
  print(i)
}


# add this information into the shapefile data drame
countriesShape$music <- countryCount
countriesShape$artists <- artistVec
which(countriesShape$NAME_LONG == "United Kingdom")
# subset the shapefile to remove those with zero entries
nonZero <- subset(countriesShape, countriesShape$music > 0)
#nonZero$music <- log(nonZero$music + 1)
# use leaflet to plot country densities
nonZero <- nonZero[rev(order(nonZero$music)),]
someCols <- plasma(length(unique(nonZero$music)))
colIndex <- match(nonZero$music, unique(nonZero$music))
# 
# # temp nonZero
# nonZero <- nonZero[1:4,]
# someCols <- plasma(length(nonZero$music))

plot1 <- leaflet(nonZero) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
         addPolygons(weight = 1, 
                     opacity = 0.1, 
                     fillColor = someCols[colIndex], 
                     popup = ~artists) %>% 
         addLegend(position = "topright", 
                   colors = someCols, 
                   labels = unique(nonZero$music))
plot1
