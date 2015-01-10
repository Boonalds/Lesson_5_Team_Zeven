# Authors: Wilmar van Ommeren & Rob Maas
# Date created: 9th of January 2015

# load libraries
library(sp)
library(rgeos)
library(rgdal)

# Load sources
  
# Download, unzip and load data
download.file(url = 'http://www.mapcruzin.com/download-shapefile/netherlands-places-shape.zip',
              destfile = 'data/places.zip',  method = 'auto')
download.file(url = 'http://www.mapcruzin.com/download-shapefile/netherlands-railways-shape.zip',
              destfile = 'data/railways.zip',  method = 'auto')

unzip('data/places.zip', exdir = "./data/places")
unzip('data/railways.zip', exdir = "./data/railways")

places.shape<-readOGR(dsn='data/places/places.shp', layer='places')
railways.shape<-readOGR(dsn='data/railways/railways.shp', layer='railways')

# Projecting the shapefiles
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
cityname <- as.character(placesRD$name[row.names(placesRD) == cityrowname][1]

# Post processing/plotting
loclabel<- c(placeswithinbuffer$x,placeswithinbuffer$y-50) # -50 to have the name just under the marker
spplot(industrialbuf, zcol='type', main = "Buffer around industrial railway and intersected city", 
       sp.layout=list(list("sp.points", placeswithinbuffer, pch=19, cex=2, col="darkgreen"),
                      list("sp.text",loclabel,cityname, cex=2),
                      list("sp.lines",industrial, col="purple")),
       scales = list(draw = TRUE), xlab="longitude", ylab="latitude",
       col.regions=c("red","yellow")
      )

# Printing population
popcity <- placesRD$population[row.names(placesRD) == cityrowname][1]
paste(cityname,"has a population of",popcity,"people")