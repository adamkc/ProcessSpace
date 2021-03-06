#' addStreamChannels
#'
#' @param transectObject Object created by the \code{generateCrossSections} function
#' @param sampleDensity The precision of the
#' @param rasterDir The file location of the DEM file.
#'
#' @return
#' @export
#'
#' @examples
addStreamChannels <- function(transectObject,
                              sampleDensity = units::as_units(6,"m"),
                              rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
                              streamDir = "GeoData/STREAM CHANNELS.shp",
                              ...){
  ##Error Checks:
  if(!is.character(rasterDir)) error("rasterDir argument needs to be a character string")
  if(!is.character(streamDir)) error("streamDir argument needs to be a character string")
  ##

  cat(crayon::green("Adding Stream Channel Data"))
  startTime <- Sys.time()
  r <- raster::raster(rasterDir)
  mapToChange <- transectObject$satImage

  bufferPoly <- sf::st_union(transectObject$leftSide,
                             transectObject$rightSide) %>%
    sf::st_bbox()
  streams = sf::read_sf(streamDir)
  if(sf::st_crs(bufferPoly) != sf::st_crs(streams)){
    cat(crayon::white("mainLine and stream crs differnt. Transforming...  "))
    streams <- streams %>% sf::st_transform(crs=sf::st_crs(bufferPoly))
  }

  sideChannelsPoints <-  sf::st_geometry(streams) %>%
    sf::st_crop(bufferPoly) %>%
    sf::st_cast("LINESTRING",warn=FALSE) %>%
    sf::st_line_sample(density = sampleDensity) %>%
    sf::st_cast("POINT",warn=FALSE) %>%
    sf::st_as_sf()

  sideChannelsPoints$Dist <- sf::st_distance(sideChannelsPoints,
                                         transectObject$mainLine%>%
                                           sf::st_union()) %>% as.numeric()
  sideChannelsPoints$DistLS <- sf::st_distance(sideChannelsPoints,
                                           transectObject$leftSide%>%
                                             sf::st_union()) %>% as.numeric()
  sideChannelsPoints$DistRS <- sf::st_distance(sideChannelsPoints,
                                           transectObject$rightSide%>%
                                             sf::st_union()) %>% as.numeric()
  sideChannelsPoints$Elevation <- raster::extract(r,sideChannelsPoints)


  plotDataSideChannels <- sideChannelsPoints %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate(Side = apply(cbind(DistLS,DistRS),1,
                               function(x) dplyr::case_when(which.min(x)==1 ~ "rs",
                                                     which.min(x)==2 ~ "ls"))) %>%
    dplyr::mutate(Dist = dplyr::case_when(Side=="ls"~-Dist,
                                          Side=="rs"~Dist))

  outputTimer(startTime)

  transectObject$plotDataSideChannels <- plotDataSideChannels
  return(transectObject)
}

