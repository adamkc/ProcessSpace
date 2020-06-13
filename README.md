## ProcessSpace

#### Adam Cummings (USFS), <adam.cummings@usda.gov>
A package to process Lidar to visualize stream channels flow paths.

### Installation

Install the package as follows:

``` r
install.packages('devtools')
library(devtools)
install_github('adamkc/ProcessSpace')
library(ProcessSpace)
```

### Inputs

1. Digital Terrain Model
2. Target Stream Reach

### Outputs

1. Algorithmic cross sections
1. Process Space delineation
1. Report export
1. Detrended elevations
1. A mindset that extends beyond the meadow surface

### Example Code:

The example below will produce a KMZ file to load into Google Earth.

``` r
library(ProcessSpace)
library(ggmap) # necessary to load credentials.

rasterDir <- system.file("external/raster.tif", package="ProcessSpace")
streamsDir <- system.file("external/streams.shp", package="ProcessSpace")
streams <- sf::read_sf(streamsDir)
targetStream <- streams %>% dplyr::filter(LINKNO %in% c(12,20))


targetStream %>%
  generateCrossSections(xSectionDensity = units::as_units(100,"m"),
                        googleZoom=16,
                        xSectionLength = units::as_units(100,"m"),
                        cut1Dir = "W",
                        cut2Dir = "E") %>% 
  allAtOnce(outputFilename = "exampleOutput.pdf",
            rasterDir = rasterDir,
            verticalCutoff=8,
            streamChannelFile = streamsDir,
            returnObject = FALSE,
            doExportSpatial = TRUE)
```

![Example image from Google Earth](images/ExampleOutput.png)
