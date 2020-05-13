#' addStreamChannels
#'
#' @param transectObject
#' @param sampleDensity
#' @param rasterDir
#'
#' @return
#' @export
#'
#' @examples
addStreamChannels <- function(transectObject,
                              sampleDensity = units::as_units(3,"m"),
                              rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
                              streamChannelFile = "GeoData/STREAM CHANNELS.shp",
                              ...){
  cat(crayon::green("Adding Stream Channel Data"))
  startTime <- Sys.time()
  r <- raster::raster(rasterDir)
  mapToChange <- transectObject$satImage

  bufferPoly <- sf::st_union(transectObject$leftSide,transectObject$rightSide) %>%
    sf::st_bbox()
  # st_polygonize() %>%
  # sf::st_transform(crs = 26910)

  sideChannelsPoints <- sf::read_sf(streamChannelFile) %>%
    sf::st_transform(crs = 26910) %>%
    #st_intersection(bufferPoly) %>%
    sf::st_crop(bufferPoly) %>%
    sf::st_cast("LINESTRING",warn=FALSE) %>%
    sf::st_line_sample(density = sampleDensity) %>%
    sf::st_cast("POINT",warn=FALSE) %>% sf::st_as_sf()

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

