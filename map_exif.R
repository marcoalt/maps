library(maps)
library(geosphere)
library(scales) #used for transparencies

picsPath <- "/Users/Marco/Dropbox/Camera Uploads/"

convertCoordinates <- function(coordinatesDegreesLat, coordinatesDegreesLon)
{
  degs <- as.numeric(coordinatesDegreesLat[[1]][4])
  mins <- as.numeric(gsub("'","",coordinatesDegreesLat[[1]][6]))
  secs <- as.numeric(gsub("\"","",coordinatesDegreesLat[[1]][7]))
  ref <- gsub("\"","",coordinatesDegreesLat[[1]][8])
  if (ref == "N")
  {
    coordinatesFloatLat <- degs + mins/60 + secs/3600
  } else {
    coordinatesFloatLat <- 0 - degs + mins/60 + secs/3600
  }
  
  degs <- as.numeric(coordinatesDegreesLon[[1]][4])
  mins <- as.numeric(gsub("'","",coordinatesDegreesLon[[1]][6]))
  secs <- as.numeric(gsub("\"","",coordinatesDegreesLon[[1]][7]))
  ref <- gsub("\"","",coordinatesDegreesLon[[1]][8])
  if (ref == "E")
  {
    coordinatesFloatLon <- degs + mins/60 + secs/3600
  } else {
    coordinatesFloatLon <- 0 - degs + mins/60 + secs/3600
  }
  
  return (list(coordinatesFloatLat, coordinatesFloatLon))
}

getDistance <- function (lat1, long1, lat2, long2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

#reference systems are not extracted since it seems the information is already included in the GPSLatitude and GPSLongitude fields
info <- system(paste("exiftool -GPSLatitude -GPSLongitude -DateTimeOriginal '", picsPath,"'",sep=""), inter=TRUE)
picsLocations <- c()
for(indexLine in 1:(length(info)-2))
{
  #latitude and longitude come sequentially (check needed since sometimes only one of the two is retrieved)
  if(length(grep("GPS Latitude", info[indexLine]) > 0) &
       length(grep("GPS Longitude", info[indexLine+1]) > 0) & 
       length(grep("Date", info[indexLine+2]) > 0))
  {
    #new pic with GPS data found - extract coordinates
    coordinatesDegreesLat <- strsplit(info[indexLine], "[ ]+")
    coordinatesDegreesLon <- strsplit(info[indexLine+1], "[ ]+")
    coordinatesFloat <- convertCoordinates(coordinatesDegreesLat, coordinatesDegreesLon)
    
    date <- strsplit(info[indexLine+2], "[ ]+")[[1]][4] #get date only
    date <- as.Date(date, "%Y:%m:%d")
    picsLocations <- rbind(picsLocations, c(coordinatesFloat[[1]], coordinatesFloat[[2]], date))
  }
}
picsLocations <- data.frame(picsLocations)
colnames(picsLocations) <- c("latitude","longitude","julianDate") #julian dates since we need only days differences

picsLocations[, "distance"] <- c(0, getDistance(picsLocations[1:(nrow(picsLocations)-1),]$latitude,
                                           picsLocations[1:(nrow(picsLocations)-1),]$longitude,
                                           picsLocations[2:nrow(picsLocations),]$latitude,
                                           picsLocations[2:nrow(picsLocations),]$longitude))

#filter out locations closer than distThreshold kilometers
distThreshold <- 300
picsLocationsReduced <- picsLocations[picsLocations$distance > distThreshold, ]
print(nrow(picsLocationsReduced))
#compute days spent in each location
picsLocationsReduced[, "days"] <- c(diff(picsLocationsReduced$julianDate), 0)
#circles look better if time spent is computed on a logarithmic scale
picsLocationsReduced[, "days"] <- log(picsLocationsReduced[, "days"])
maxDays <- max(picsLocationsReduced$days, na.rm = TRUE)

scaleFactorCircles <- 5
alphaLevel <- 0.8
pal <- colorRampPalette(c("#333333", "#12c4db", "#1292db"))
colors <- pal(100)
pdf(paste(picsPath,"map.pdf",sep=""), width=22, height=14)
map("world", col="#1b1b1b", fill=TRUE, bg="#000000", lwd=0.05)#, xlim=xlim, ylim=ylim)
for (indexLoc in 1:(nrow(picsLocationsReduced)-1))
{
  currLat <- picsLocationsReduced[indexLoc,]$latitude
  currLon <- picsLocationsReduced[indexLoc,]$longitude
  nextLat <- picsLocationsReduced[(indexLoc+1),]$latitude
  nextLon <- picsLocationsReduced[(indexLoc+1),]$longitude
  daysDiff <- picsLocationsReduced[(indexLoc+1),]$julianDate - picsLocationsReduced[(indexLoc),]$julianDate
  
  inter <- gcIntermediate(c(currLon, currLat), c(nextLon, nextLat), n = 100, addStartEnd = TRUE)
  #lines color changes sequentially
  colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
  lines(inter, col = colors[colindex], lwd = 2) 
  #points color and size changes based on time spent in each location
  colindex <- round( picsLocationsReduced[indexLoc,]$days/maxDays * length(colors) )
  points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$days/maxDays*scaleFactorCircles)
}
