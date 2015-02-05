library(maps)
library(geosphere)
library(scales) #used for transparencies

picsPath <- "/Users/Marco/Dropbox/Camera Uploads/marco2012_2015/"

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
    year <- as.numeric(substr(date, 0, 4))
    date <- as.Date(date, "%Y:%m:%d")
    picsLocations <- rbind(picsLocations, c(coordinatesFloat[[1]], coordinatesFloat[[2]], date, year))
  }
}
picsLocations <- data.frame(picsLocations)
colnames(picsLocations) <- c("latitude","longitude","julianDate", "year") #julian dates since we need only days differences

#uncomment to set starting date
#firstDate <- as.Date("2015:01:01", "%Y:%m:%d")
#picsLocations <- picsLocations[picsLocations$julianDate >= firstDate, ]

picsLocations$year <- as.factor(picsLocations$year)

#order by date (should be already ordered)
picsLocations <- picsLocations[order(picsLocations$julianDate), ]

picsLocations[, "distance"] <- c(0, getDistance(picsLocations[1:(nrow(picsLocations)-1),]$latitude,
                                                picsLocations[1:(nrow(picsLocations)-1),]$longitude,
                                                picsLocations[2:nrow(picsLocations),]$latitude,
                                                picsLocations[2:nrow(picsLocations),]$longitude))

#filter out locations closer than distThreshold kilometers
#compute time spent in each location
#determine number of pics taken per location
distThreshold <- 300
picsLocationsReduced <- c()
#do this in a loop to keep track of how many pictures were taken
picsHere <- 1
timeHere <- 1
for(indexPic in 2:nrow(picsLocations))
{
  if(picsLocations[indexPic, ]$distance > distThreshold)
  {
    picsHere <- picsHere + 1
    timeHere <- timeHere + picsLocations[indexPic, ]$julianDate - picsLocations[(indexPic-1), ]$julianDate
    picsOverTime <- round(picsHere/timeHere, 1)
    picsLocationsReduced <- rbind(picsLocationsReduced, cbind(picsLocations[(indexPic-1), ], picsHere, timeHere, picsOverTime) )
    #reset variables
    picsHere <- 1
    timeHere <- 1
    ##add last one
    if(indexPic == nrow(picsLocations))
    {
      picsLocationsReduced <- rbind(picsLocationsReduced, cbind(picsLocations[indexPic, ], picsHere=1, timeHere=1, picsOverTime=1) )
    }
  } else {
    picsHere <- picsHere + 1
    timeHere <- timeHere + picsLocations[indexPic, ]$julianDate - picsLocations[(indexPic-1), ]$julianDate
    picsOverTime <- round(picsHere/timeHere, 1)
    ##add last one
    if(indexPic == nrow(picsLocations))
    {
      picsLocationsReduced <- rbind(picsLocationsReduced, cbind(picsLocations[indexPic, ], picsHere, timeHere, picsOverTime) )
    }
  }
}

#plot time spent in each location + great cricles between locations
#better plotting over logarithmic scale
picsLocationsReduced[, "timeHere"] <- log(picsLocationsReduced$timeHere)
maxDays <- max(picsLocationsReduced$timeHere, na.rm = TRUE)

line_width <- 1
scaleFactorCircles <- 3
alphaLevel <- 0.8
pal <- colorRampPalette(c("#333333", "#12c4db", "#1292db"))
colors <- pal(100)
pdf(paste(picsPath,"mapTime.pdf",sep=""), width=11, height=7)

map("world", col="#1b1b1b", fill=TRUE, bg="#000000", lwd=0.05)#, xlim=xlim, ylim=ylim)
for (indexLoc in 1:(nrow(picsLocationsReduced)-1))
{
  currLat <- picsLocationsReduced[indexLoc,]$latitude
  currLon <- picsLocationsReduced[indexLoc,]$longitude
  nextLat <- picsLocationsReduced[(indexLoc+1),]$latitude
  nextLon <- picsLocationsReduced[(indexLoc+1),]$longitude
  daysDiff <- picsLocationsReduced[(indexLoc+1),]$julianDate - picsLocationsReduced[(indexLoc),]$julianDate
  
  #if(currLon < 0 &)
  inter <- gcIntermediate(c(currLon, currLat), c(nextLon, nextLat), n = 150, addStartEnd = TRUE, breakAtDateLine = TRUE)
  if(length(inter) != 2) #there is no crossing of the dateline
  {
    #lines color changes sequentially
    colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
    lines(inter, col = colors[colindex], lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$timeHere/maxDays * length(colors) )
    points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$timeHere/maxDays*scaleFactorCircles)
  } else {
    interFirstHalf <- inter[[1]]
    colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
    lines(interFirstHalf, col = colors[colindex], lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$timeHere/maxDays * length(colors) )
    points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$timeHere/maxDays*scaleFactorCircles)
    interSecondHalf <- inter[[2]]
    colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
    lines(interSecondHalf, col = colors[colindex], lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$timeHere/maxDays * length(colors) )
    points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$timeHere/maxDays*scaleFactorCircles)
    }
}

dev.off()

#plot number of pics in each location (normalized by time spent) + great cricles between locations
picsLocationsReduced[, "picsOverTime"] <- log(picsLocationsReduced[, "picsOverTime"])
maxPics <- max(picsLocationsReduced$picsOverTime, na.rm = TRUE)

pal <- colorRampPalette(c("#333333", "#ffe400", "#ff9c00"))
colors <- pal(100)
pdf(paste(picsPath,"mapNumPics.pdf",sep=""), width=11, height=7)

map("world", col="#1b1b1b", fill=TRUE, bg="#000000", lwd=0.05)#, xlim=xlim, ylim=ylim)
for (indexLoc in 1:(nrow(picsLocationsReduced)-1))
{
  currLat <- picsLocationsReduced[indexLoc,]$latitude
  currLon <- picsLocationsReduced[indexLoc,]$longitude
  nextLat <- picsLocationsReduced[(indexLoc+1),]$latitude
  nextLon <- picsLocationsReduced[(indexLoc+1),]$longitude
  daysDiff <- picsLocationsReduced[(indexLoc+1),]$julianDate - picsLocationsReduced[(indexLoc),]$julianDate
  
  inter <- gcIntermediate(c(currLon, currLat), c(nextLon, nextLat), n = 150, addStartEnd = TRUE, breakAtDateLine = TRUE)
  if(length(inter) != 2) #there is no crossing of the dateline
  {
    #lines color changes sequentially
    colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
    lines(inter, col = colors[colindex], lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$picsOverTime/maxPics * length(colors) )
    points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$picsOverTime/maxPics*scaleFactorCircles)
  } else {
    interFirstHalf <- inter[[1]]
    colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
    lines(interFirstHalf, col = colors[colindex], lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$picsOverTime/maxPics * length(colors) )
    points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$picsOverTime/maxPics*scaleFactorCircles)
    interSecondHalf <- inter[[2]]
    colindex <- round( (indexLoc / nrow(picsLocationsReduced)) * length(colors) )
    lines(interSecondHalf, col = colors[colindex], lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$picsOverTime/maxPics * length(colors) )
    points(currLon, currLat, col = alpha(colors[colindex], alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$picsOverTime/maxPics*scaleFactorCircles)
  }
}

dev.off()


##plot different colors per year
#plot time spent in each location + great cricles between locations
#better plotting over logarithmic scale
colors_years = c("brown1", "bisque1", "aquamarine1", "darkgoldenrod1", "darkolivegreen1", "azure3", "coral", "cadetblue1", "ghostwhite", "khaki1", "lightpink")
scaleFactorCircles <- 3
alphaLevel <- 0.6
pdf(paste(picsPath,"mapYear.pdf",sep=""), width=11, height=7)
map("world", col="#1b1b1b", fill=TRUE, bg="#000000", lwd=0.05)#, xlim=xlim, ylim=ylim)
for (indexLoc in 1:(nrow(picsLocationsReduced)-1))
{
  currLat <- picsLocationsReduced[indexLoc,]$latitude
  currLon <- picsLocationsReduced[indexLoc,]$longitude
  nextLat <- picsLocationsReduced[(indexLoc+1),]$latitude
  nextLon <- picsLocationsReduced[(indexLoc+1),]$longitude
  daysDiff <- picsLocationsReduced[(indexLoc+1),]$julianDate - picsLocationsReduced[(indexLoc),]$julianDate
  
  currcolor = colors_years[levels(picsLocationsReduced$year) == picsLocationsReduced[(indexLoc),]$year]
  
  #if(currLon < 0 &)
  inter <- gcIntermediate(c(currLon, currLat), c(nextLon, nextLat), n = 150, addStartEnd = TRUE, breakAtDateLine = TRUE)
  if(length(inter) != 2) #there is no crossing of the dateline
  {
    lines(inter, col = alpha(currcolor, alphaLevel), lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$timeHere/maxDays * length(colors) )
    points(currLon, currLat, col = alpha(currcolor, alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$timeHere/maxDays*scaleFactorCircles)
  } else {
    interFirstHalf <- inter[[1]]
    lines(interFirstHalf, col = alpha(currcolor, alphaLevel), lwd = line_width) 
    #points color and size changes based on time spent in each location
    points(currLon, currLat, col = alpha(currcolor, alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$timeHere/maxDays*scaleFactorCircles)
    interSecondHalf <- inter[[2]]
    lines(interSecondHalf, col = alpha(currcolor, alphaLevel), lwd = line_width) 
    #points color and size changes based on time spent in each location
    colindex <- round( picsLocationsReduced[indexLoc,]$timeHere/maxDays * length(colors) )
    points(currLon, currLat, col = alpha(currcolor, alphaLevel), pch=16, cex = picsLocationsReduced[indexLoc,]$timeHere/maxDays*scaleFactorCircles)
  }
}
legend("topright", inset=.05, title="",
       levels(picsLocationsReduced$year),
       text.col=colors_years[1:length(levels(picsLocationsReduced$year))],
       fill=colors_years[1:length(levels(picsLocationsReduced$year))],
       bty = "n")
dev.off()
