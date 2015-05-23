# R script for mapping addresses. Uses Google API for coordinates.

# Dependencies : RCurl, PBSmapping, RJSONIO, maptools

# requires file w/ list of addresses in csv format. File should be 
# saved as "cms.csv" and located in the working directory


# (c) Rebeca Carrillo-Clayton, 2015
# rc@rcclayton.com

library(PBSmapping)
library(maptools)
library(RCurl)
library(RJSONIO)

# Installs and loads a package if necessary
# to do: de-bug this
EnsurePackage<-function(x)
{
  x <- as.character(x)
  if (!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x)
    require(x, character.only=TRUE)
  }
}
# Load packages for working with twitteR
PrepareTwitter<-function()
{
  EnsurePackage("PBSmapping")
  EnsurePackage("RCurl")
  EnsurePackage("maptools")
  EnsurePackage("RJSONIO")
  
}

# Read Data and plot US map
usShape <- importShapefile("gz_2010_us_040_00_500k.shp", readDBF = T)
plotPolys(usShape)

# Eliminate Alaska and Hawaii
plotPolys(usShape, xlim = c(-130, -60), ylim = c(20, 50))

# Add a point to the map
# X: longitude Y: latitude
# EID: Event ID
pointData <- data.frame(EID = 1, X = -106.46, Y = 32.18)

eventData <- as.EventData(pointData, projection = NA)
addPoints(eventData, col = "red")


# get address data

cms <- read.csv ("cms.csv")
contractorAddr <- unique(cms$Contractor.Address)
head(contractorAddr)

# Create function to prepare URL for Google Maps API
MakeGeoURL <- function(address) 
{ 
  root <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- paste(root, address, "&sensor=false", sep = "")
  return(URLencode(url))
}

# Create funtion to convert an address to its geographic coordinates
addr2latlng <- function(address)
{
  url <- MakeGeoURL(address)
  geoStruct <- fromJSON(getURL(url), simplify = F)
  latitude <- geoStruct$results[[1]]$geometry$location$lat
  longitude <- geoStruct$results[[1]]$geometry$location$lng
  return(c(latitude, longitude))
}

testData <- addr2latlng("New Mexico State University, Las Cruces, NM")
testData

coords <- lapply(contractorAddr, addr2latlng)
head(coords)

coords <- data.frame(matrix(unlist(coords), ncol = 2, byrow = T))

pointData <- cbind(EID = 1:nrow(coords), X = coords[,2], Y = coords [,1])

eventData <- as.EventData(pointData, projection = NA)
addpoints(eventData, col="red")
