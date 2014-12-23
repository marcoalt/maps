Map your trips using pics from your phone 
====

This script provides R code to visualize GPS data using getoagged pictures. 

Most pictures taken using a mobile phone get automatically geotagged. This script extracts metadata from JPEG files, including latitude, longitude and date when the picture was taken, and plots great circles between locations farther then a configurable threshold. Additionally, time spent in different locations is shown using circles of different sizes. 

Parts of the script are partially based on code from flowing data / Nathan Yau (http://flowingdata.com/2011/05/05/where-do-major-airlines-fly-in-the-united-states/) and timelyportfolio (http://timelyportfolio.github.io/rCharts_catcorrjs/exif/) 

Usage:
- download and install exiftool, which is called by the R script (http://www.sno.phy.queensu.ca/~phil/exiftool/)
- download your pics from your mobile phone (I used Dropbox Camera Upload)
- set the path to your pics folder in the R script
- run the script
