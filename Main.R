# Authors: Wilmar van Ommeren & Rob Maas
# Date created: 9th of January 2015

# load libraries
library(sp)
library(rgeos)
library(rgdal)

# Load sources - as everything is very sequential we did not create seperate functions
  
# Download, unzip and load data
download.file(url = 'http://www.mapcruzin.com/download-shapefile/netherlands-places-shape.zip',
              destfile = 'data/places.zip',  method = 'auto')
download.file(url = 'http://www.mapcruzin.com/download-shapefile/netherlands-railways-shape.zip',
              destfile = 'data/railways.zip',  method = 'auto')

unzip('data/places.zip', exdir = "./data/places")
unzip('data/railways.zip', exdir = "./data/railways")

places.shape<-readOGR(dsn='data/places/places.shp', layer='places')
railways.shape<-readOGR(dsn='data/railways/railways.shp', layer='railways')

# Projecting the shapefiles to the RD system
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
railwaysRD <- spTransform(railways.shape, prj_string_RD)
placesRD <- spTransform(places.shape, prj_string_RD)

# Selecting industrial railways and creating a buffer around them
industrial <- railwaysRD[railwaysRD$type == 'industrial',]
industrialbuf <- gBuffer(industrial, byid=T,width=1000)

# Intersect buffer with places dataset to find city name
placeswithinbuffer <- gIntersection(placesRD,industrialbuf, byid=T)
rownames<-row.names(placeswithinbuffer)
cityrowname<-unlist(strsplit(rownames, split=' '))[1]  #The city-ID was returned as a string "5973 0".
cityname <- as.character(placesRD$name[row.names(placesRD) == cityrowname])[1]

# Post-processing
loclabel<- c(placeswithinbuffer$x,placeswithinbuffer$y+80) # +80 to have the name just above the marker
industrialbuf$legend.labels <- factor(industrialbuf$type, levels = 0:2) # Create an extra variable in the dataframe. The number of levels is the numbers of attributes displayed in the legend
labelno <- c(1,2,3) # Numbers of the attributes in the legend that should get a label
labeltext <- c('Buffer (1000 m)','City','Industrial railway')# Labels for the legend

# Plotting
spplot(industrialbuf, zcol='legend.labels', main = "Buffer around industrial railway, and intersected city", 
       sp.layout=list(list("sp.polygons", industrialbuf, fill = "gray", col = "dimgray"),
                      list("sp.points", placeswithinbuffer, pch=19, cex=2, col="blue"),
                      list("sp.text",loclabel,cityname, cex=1.5),
                      list("sp.lines",industrial, col="red"),
                      list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(134400,456400), 
                           scale = 300, fill=c("transparent","black"))
                      ),
       scales = list(draw = TRUE), xlab="Longitude", ylab="Latitude", 
       col.regions = c("gray","blue","red"), 
       colorkey = list(width = 2, height = 0.25, labels = list(at=labelno, labels=labeltext))
       )

# Printing population
popcity <- placesRD$population[row.names(placesRD) == cityrowname][1]
paste(cityname,"has a population of",popcity,"people")