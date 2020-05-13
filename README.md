# ProcessSpace
A package to process Lidar for meadow restoration visualizations.

## Inputs

1. Digital Terrain Model
2. Target Stream Reach

## Outputs

1. Algorithmic cross sections
1. Process Space delineation
1. Report export
1. Detrended elevations
1. A mindset that extends beyond the meadow surface

## Example Code:

    library(ProcessSpace)
    library(ggmap) # necessary to load credentials.
    library(patchwork) # Necessary in "buildXSectionPlot.r" for combining plots. Replace with cowplot...
    library(plotKML) #Necessary to load saga_pal
    setwd("G:/Yosemite")

    AllStreams <- sf::read_sf("GeoData/yosemite_be_net.shp") %>%
      sf::st_transform(crs = 26910)
  
      AllStreams %>%
      dplyr::filter(LINKNO == 164) %>%
      generateCrossSections(xSectionDensity = units::as_units(20,"m"),
                            googleZoom=13, xSectionLength = 200,
                            cut1Dir = "N", cut2Dir = "S") %>%
      allAtOnce(outputFilename = "MainChannel_FULL.pdf",
                rasterDir="GeoData/output_be.tif",
                streamChannelFile = "GeoData/yosemite_be_net.shp",
                doExportSpatial = TRUE,returnObject = FALSE)
