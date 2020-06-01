
######################

library(ProcessSpace)
library(ggmap) # necessary to load credentials.
# library(tidyverse)
# library(sf)
# library(ggmap)
# library(raster)
# library(rgdal)
library(patchwork) # Necessary in "buildXSectionPlot.r" for combining plots. Replace with cowplot...
 library(plotKML) #Necessary to load saga_pal
#source("R Scripts/Functions.R")

setwd("F:/Adam Cummings/ChildsMeadowGeo")

#MainChannel.shp:
mainChannel <- sf::read_sf("GeoData/MainChannel.shp") %>%
  sf::st_transform(crs = 26910) #%>% sf::st_union()

#Full Thing:  ---------------------------------------------------------
mainOutput <- mainChannel %>%
  generateCrossSections(xSectionDensity = units::as_units(20,"m"),
                        googleZoom=13, xSectionLength = 200,
                        cut1Dir = "N", cut2Dir = "S") %>%
  allAtOnce(outputFilename = "MainChannel_FULL.pdf",
            doExportSpatial = TRUE,returnObject = TRUE)
exportSpatials(mainOutput,folderName = "MainOutput")


##Subsets:---------------------------------------------------------
#x <- st_length(mainChannel)
##Addins: Copy x to clipboard, paste to excel, pick units ~1km in length:

generateCrossSections(mainChannel %>% dplyr::slice(1:2),
                      xSectionDensity = units::as_units(10,"m"),
                      googleZoom=16, xSectionLength = 150,
                      cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("mainChannelA150x10.pdf",doExportSpatial = TRUE)

generateCrossSections(mainChannel %>% dplyr::slice(3:4),
                      xSectionDensity = units::as_units(10,"m"),
                      googleZoom=16, xSectionLength = 150,
                      cut1Dir = "W", cut2Dir = "S") %>%
  allAtOnce("mainChannelB150x10.pdf",doExportSpatial = TRUE)

generateCrossSections(mainChannel %>% dplyr::slice(5:6),
                      xSectionDensity = units::as_units(10,"m"),
                      googleZoom=16, xSectionLength = 150,
                      cut1Dir = "W", cut2Dir = "S") %>%
  allAtOnce("mainChannelC150x10.pdf",doExportSpatial = TRUE)

generateCrossSections(mainChannel %>% dplyr::slice(7:8),
                      xSectionDensity = units::as_units(10,"m"),
                      googleZoom=16, xSectionLength = 150,
                      cut1Dir = "W", cut2Dir = "S") %>%
  allAtOnce("mainChannelD150x10.pdf",doExportSpatial = TRUE)

# generateCrossSections(mainChannel %>% slice(1:8),
#                       xSectionDensity = units::as_units(20,"m"),
#                       googleZoom=16, xSectionLength = 150,
#                       cut1Dir = "N", cut2Dir = "S") %>%
#   allAtOnce("mainChannelA-D.pdf",doExportSpatial = TRUE)

generateCrossSections(mainChannel %>% dplyr::slice(9:12),
                      xSectionDensity = units::as_units(10,"m"),
                      googleZoom=16, xSectionLength = 150,
                      cut1Dir = "N", cut2Dir = "S") %>%
  allAtOnce("mainChannelE150x10.pdf",doExportSpatial = TRUE)

mainF <- generateCrossSections(mainChannel %>% dplyr::slice(13:14),
                               xSectionDensity = units::as_units(10,"m"),
                               googleZoom=16, xSectionLength = 150,
                               cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("mainChannelF150x10.pdf",doExportSpatial = TRUE,returnObject = TRUE)

# generateCrossSections(mainChannel %>% slice(9:15),
#                       xSectionDensity = units::as_units(15,"m"),
#                       googleZoom=15, xSectionLength = 150,
#                       cut1Dir = "N", cut2Dir = "E") %>%
#   allAtOnce("mainChannelE-G.pdf",doExportSpatial = TRUE)

generateCrossSections(mainChannel %>% dplyr::slice(15:16),
                      xSectionDensity = units::as_units(10,"m"),
                      googleZoom=16, xSectionLength = 150,
                      cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("mainChannelG150x10.pdf",doExportSpatial = TRUE)



#exportSpatials(finger1,sectionName = "finger1")
#plotKML::kml_compress("finger1.kml")
Sys.time()

#  mapPlotter() %>%
# #exportSpatials(folderName = "finger1b")
# #longitudinalElevation(plotFileName = "Finger1LongitudinalPIPE.png") %>%
# addTopoLines() %>%
# addStreamChannels() %>%
# addCrossSectionElevations() %>%
# addProcessSpace()

#finger1Output <-  addProcessSpace(finger1Output)

# buildXSectionPlot(finger1Output,plotFileName = "Finger1XsectionPIPE2.pdf")


#### North Finger-----------------------------------------------
sf::read_sf("GeoData/WoodyFingerNChannel.shp") %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=15,
                        xSectionLength = 100,
                        xSectionDensity = units::as_units(25,"m"),
                        cut1Dir = "N",
                        cut2Dir = "E") %>%
  allAtOnce("WoodyFingerN.pdf",doExportSpatial = TRUE)


#WoodyFingerN2.shp:---------------------------------------------------------
woodyFingerN2 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(1548,1554,1562,1568,1576,1596,1609,1643)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 150,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerN2_150x5.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)


#WoodyFingerN2b.shp:---------------------------------------------------------
woodyFingerN2b <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(1646,1648,1649)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 150,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerN2b_150x5.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)


#WoodyFingerN3.shp:---------------------------------------------------------
woodyFingerN3 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(1627,1635,1649,1709,1638)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 150,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerN3_150x5.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)

#WoodyFingerN4.shp:---------------------------------------------------------
woodyFingerN4 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  dplyr::filter(arcid %in% c(1638,1741)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 150,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerN4_150x5.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)


#WoodyFingerN5.shp:---------------------------------------------------------
woodyFingerN5 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  dplyr::filter(arcid %in% c(1748,1761,1789,1905)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 150,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerN5_150x5.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)


#WoodyFingerS1.shp:---------------------------------------------------------
#sf::read_sf("GeoData/WoodyFingerS1.shp") %>% plot
sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(2143,2181,2187,2193,2205,2197,2211,#2227,2239,2254,
                      2206,2257)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 200,
                        xSectionDensity = units::as_units(20,"m"),
                        cut1Dir = "W",
                        cut2Dir = "E") %>%
  allAtOnce("WoodyFingerS1_200x20.pdf",doExportSpatial = TRUE)





#WoodyFingerS2.shp:---------------------------------------------------------
sf::read_sf("GeoData/WoodyFingerS2.shp") %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 200,
                        xSectionDensity = units::as_units(20,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerS2.pdf",doExportSpatial=TRUE)

exportSpatials(output,folderName = "WoodyFingerS2")



#WoodyFingerS3.shp:---------------------------------------------------------
woodyFingerS3 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(2221,2270,2280,2407,2257)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 100,
                        xSectionDensity = units::as_units(10,"m"),
                        cut1Dir = "W", cut2Dir = "E") %>%
  allAtOnce("WoodyFingerS3.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)

#-
#LostMeadow:---------------------------------------------------------
LostM1 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(4069,4093,4144,4147,4194,4282,4487,4553,4690)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 100,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "W", cut2Dir = "S") %>%
  allAtOnce("LostM1.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)


#LostMeadow:------------------------------------------------
LostM2 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
  filter(arcid %in% c(4137,4250,4277,4389,4436,4444,4473,4536,4611,4723,4892)) %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 100,
                        xSectionDensity = units::as_units(5,"m"),
                        cut1Dir = "N", cut2Dir = "S") %>%
  allAtOnce("LostM2.pdf",doExportSpatial=TRUE,
            detrendElevs=TRUE,returnObject = TRUE)



### finger1-------------♦-----------------------------


finger1 <- sf::read_sf("GeoData/Finger1.shp") %>%
  sf::st_transform(crs = 26910) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = units::as_units(100,"m"),
                        xSectionDensity = units::as_units(20,"m")) %>%
  allAtOnce("Finger1.pdf",
            doExportSpatial = TRUE,
            returnObject = TRUE)

sf::read_sf("RasterOutputs/ConceptualPlan/CulvertsPlusDiversions/raster_net.shp") %>%
  sf::st_transform(crs = 26910) %>%
  filter(LINKNO == 34) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 200,
                        xSectionDensity =units::as_units(20,"m"),
                        cut1Dir = "W",cut2Dir = "E") %>%
  allAtOnce("FingerReroute.pdf",
            doExportSpatial = TRUE)


# finger1 <- sf::read_sf("GeoData/STREAM CHANNELS.shp") %>%
#   filter(arcid %in% c(1156,1168,1183,1195,
#                       1203,1207,1210,1216,
#                       1217,1220,1221,1223,
#                       1224)) %>%
#   sf::st_transform(crs = 26910) %>%
#   generateCrossSections(googleZoom=16,
#                         xSectionLength = as_units(100,"m"),
#                         xSectionDensity = as_units(10,"m")) %>%
#   allAtOnce("Finger1Upper.pdf",
#             doExportSpatial = TRUE,
#             returnObject = TRUE)
#
#
#
# finger1 <- sf::read_sf("GeoData/Finger1.shp") %>%
#   sf::st_transform(crs = 26910) %>%
#   slice(c(1,3,4,9,11,10)) %>%
#   generateCrossSections(googleZoom=16,
#                         xSectionLength = 200,
#                         xSectionDensity =units::as_units(5,"m")) %>%
#   allAtOnce("Finger1.pdf",
#             doExportSpatial = TRUE,
#             returnObject = TRUE)


##North of Poppy's---------------------------------------------
sf::read_sf("RasterOutputs/ConceptualPlan/Culverts/raster_net.shp") %>%
  sf::st_transform(crs = 26910) %>%
  filter(LINKNO == 40) %>%
  generateCrossSections(googleZoom=16,
                        xSectionLength = 200,
                        xSectionDensity =units::as_units(20,"m")) %>%
  allAtOnce("NofPoppys.pdf",
            doExportSpatial = TRUE)
