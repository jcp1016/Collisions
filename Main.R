setwd(".")
#library("devtools")
#devtools::install_github("/ropensci/plotly")

library("Cairo")
library("dplyr")
library("extrafont")
library("ggmap")
library("ggplot2")
library("ggthemes")
library("gpclib")
library("graphics")
library("grDevices")
library("grid")
library("gridExtra")
#library("plotly")
library("RColorBrewer")
library("lubridate")
library("maptools")
library("reshape2")
library("rgdal")
library("rgeos")
library("sp")
library("sqldf")

rawdata <- read.csv("./DATA/NYPD_Motor_Vehicle_Collisions-2014.csv", header=TRUE)
names(rawdata)[4] <- "ZIP"
rawdata$DATE <- as.character(rawdata$DATE)

data <- sqldf("select ZIP, 
              count(*) as 'Collisions', 
              sum(`NUMBER.OF.PERSONS.KILLED`) as 'Fatalities', 
              sum(`NUMBER.OF.CYCLIST.KILLED`) as 'BicycleFatalities', 
              sum(`NUMBER.OF.CYCLIST.INJURED`) as 'BicycleInjuries' 
              from rawdata 
              group by ZIP")

rawdata$PDATE <- mdy(rawdata$DATE)
rawdata$DAY   <- wday(rawdata$PDATE, label=TRUE, abbr=TRUE)

ftotal <- sqldf("select sum(`NUMBER.OF.PERSONS.KILLED`) from rawdata")

data_by_day   <- sqldf("select DAY, 
                       sum(`NUMBER.OF.PERSONS.KILLED`) as 'Fatalities' 
                       from rawdata 
                       group by DAY")
data_by_day$Pct <- (data_by_day$Fatalities / as.numeric(ftotal)) * 100

## Get Map
setwd("./zip_codes_shp")
zipmap <- readOGR(dsn=".","PostalBoundary")
zipmap_nyc <- zipmap[zipmap$POSTAL %in% factor(data$ZIP),]
zipmap_nyc <- spTransform(zipmap_nyc, CRS("+proj=longlat + datum=WGS84"))
setwd("../")

## Merge map with data by zip
zipmap_nyc_df <- fortify(zipmap_nyc)
map_data <- data.frame(id=rownames(zipmap_nyc@data), ZIP=zipmap_nyc@data$POSTAL, ZIPNAME=zipmap_nyc@data$NAME)
map_data <- merge(map_data, data, by="ZIP")
map_df   <- merge(zipmap_nyc_df, map_data, by="id")

map_df$CollisionRange <- cut(map_df$Collisions, breaks=quantile(map_df$Collisions), dig.lab=4, include.lowest=TRUE)

## Make plots
p1 <- qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=CollisionRange), 
                     data=map_df, 
                     color='white',
                     alpha=.6, 
                     size=.3) + 
        ggtitle("Collisions Reported in New York City - 2014") +
        scale_fill_brewer("Collision Range") +
        theme(plot.title = element_text(size=16, face="bold"))

p2 <- qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=factor(Fatalities)),
                     data=map_df,
                     color='white',
                     alpha=.6, 
                     size=.3) + 
        ggtitle("Collisions Reported in New York City - 2014") +
        scale_fill_brewer("Fatalities") +
        theme(plot.title = element_text(size=16, face="bold"))

p3 <- qmap('new york, ny', zoom=11, maptype='roadmap', color='bw', legend='topleft') +
        geom_polygon(aes(long, lat, group=group, fill=factor(BicycleFatalities)),
                     data=map_df,
                     color='white',
                     alpha=.6, 
                     size=.3) + 
        ggtitle("Collisions Reported in New York City - 2014") +
        scale_fill_brewer("Bicycle Fatalities") +
        theme(plot.title = element_text(size=16, face="bold"))

p4 <- ggplot(data_by_day, aes(x=DAY, y=Pct, fill=Pct)) + 
        geom_bar(stat="identity") + 
        scale_fill_gradient(low="#aaaaaa", high="#333333") + 
        ggtitle("Fatal Collisions by Day of the Week") + 
        theme_fivethirtyeight() + 
        xlab(NULL) + ylab("% of Total") + 
        guides(fill=FALSE) + 
        scale_color_fivethirtyeight()

## Save to svg files
svg(filename = "AllCollisionsPlot.svg", width=7, height=7, onefile=TRUE, pointsize=12, family="sans", bg="white", antialias=c("default", "none", "gray", "subpixel"))
print(p1)
dev.off()

svg(filename = "FatalitiesPlot.svg", width=7, height=7, onefile=TRUE, pointsize=12, family="sans", bg="white", antialias=c("default", "none", "gray", "subpixel"))
print(p2)
dev.off()

svg(filename = "BikeFatalitiesPlot.svg", width=7, height=7, onefile=TRUE, pointsize=12, family="sans", bg="white", antialias=c("default", "none", "gray", "subpixel"))
print(p3)
dev.off()

svg(filename = "FatalitiesByDay.svg", width=7, height=7, onefile=TRUE, pointsize=12, family="sans", bg="white", antialias=c("default", "none", "gray", "subpixel"))
print(p4)
dev.off()

## Show plots
print(p1)
print(p2)
print(p3)
print(p4)

## Upload to plotly
#py <- plotly()
#py$ggplotly(p1, kwargs=list(world_readable=FALSE))