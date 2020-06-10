#' Generate Stream Cross Sections
#'
#' @param streamChannel
#' @param getSatImage
#' @param googleZoom
#' @param cut1Dir
#' @param cut2Dir
#' @param xSectionLength
#' @param xSectionDensity
#'
#' @return
#' @export
#'
#' @examples
#'
generateCrossSections <- function(streamChannel,
                                  getSatImage=TRUE,
                                  googleZoom = 14,
                                  cut1Dir = "W",
                                  cut2Dir = "E",
                                  xSectionLength = 100,
                                  xSectionDensity = units::as_units(100,"m")){

  plotBbox.WGS <- sf::st_bbox(streamChannel %>%
                                sf::st_transform(crs=4326))

  streamChannel.union <- sf::st_union(streamChannel) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_line_merge()%>%
    sf::st_cast("LINESTRING",warn=FALSE)
  #streamChannel.union$Shape_Leng = st_length(streamChannel.union)

  channelLength = sum(sf::st_length(streamChannel.union))
  #gapBetweenTransects <- 100
  #pointDensity = 0.01
  sampledPoints <- sf::st_line_sample(streamChannel.union,# %>% slice(3:12),
                                      density= xSectionDensity,
                                      #n =ceiling(xSectionCount),
                                      type="regular") %>%
    sf::st_cast("POINT",warn=FALSE) %>%
    data.frame() %>%
    dplyr::mutate(pointID = 1:dplyr::n()) %>%
    sf::st_as_sf()
  ####


  # buff1 <- sf::st_buffer(streamChannel %>% sf::st_union(),
  #                        dist = xSectionLength,nQuadSegs = 100,
  #                        singleSide=TRUE) %>%
  #   smoothr::smooth("ksmooth",smoothness=20) %>%
  #   st_cast("MULTILINESTRING",warn=FALSE) %>%
  #   sf::st_difference(y=sf::st_buffer(streamChannel %>% sf::st_union(),
  #                                       dist = xSectionLength-5,
  #                                     nQuadSegs = 100)) %>%
  #   st_cast("MULTILINESTRING")
  #
  # buff2 <- sf::st_buffer(streamChannel %>% sf::st_union(),
  #                        dist = -xSectionLength,nQuadSegs = 100,
  #                        singleSide=TRUE)%>%
  #   st_cast("MULTILINESTRING",warn=FALSE) %>%
  #   st_line_merge() %>%
  #   sf::st_difference(y=sf::st_buffer(streamChannel %>% sf::st_union(),
  #                                     dist = xSectionLength-5,
  #                                     nQuadSegs = 100))%>%
  #   st_cast("MULTILINESTRING")
  #
  # leftSide <- buff1
  # rightSide <- buff2


  # buff1.center <- st_centroid(buff1) %>% st_coordinates
  # buff2.center <- st_centroid(buff2) %>% st_coordinates
  #
  # if(cut1Dir == "W"){
  #   if(buff1.center[2]<buff2.center[2]){
  #     rightSide <- buff1
  #     leftSide <- buff2
  #   }else{
  #     rightSide <- buff2
  #     leftSide <- buff1
  #   }
  # }
  # if(cut1Dir == "E"){
  #   if(buff1.center[2]>buff2.center[2]){
  #     rightSide <- buff1
  #     leftSide <- buff2
  #   }else{
  #     rightSide <- buff2
  #     leftSide <- buff1
  #   }
  # }
  # if(cut1Dir == "N"){
  #   if(buff1.center[1]<buff2.center[1]){
  #     rightSide <- buff1
  #     leftSide <- buff2
  #   }else{
  #     rightSide <- buff2
  #     leftSide <- buff1
  #   }
  # }
  # if(cut1Dir == "S"){
  #   if(buff1.center[1]>buff2.center[1]){
  #     rightSide <- buff1
  #     leftSide <- buff2
  #   }else{
  #     rightSide <- buff2
  #     leftSide <- buff1
  #   }
  # }


  Buff.Line <-
    sf::st_buffer(streamChannel %>% sf::st_union(),
                  dist = xSectionLength,nQuadSegs = 100) %>%
    smoothr::smooth("ksmooth",smoothness=20) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE)

  Buff.bbox <- sf::st_bbox(Buff.Line)

  if(cut1Dir == "W"){
    cut1.far <- sf::st_point(c(Buff.bbox["xmin"]-10000,
                           mean(Buff.bbox[c("ymin","ymax")])))}
  if(cut1Dir == "E"){
    cut1.far <- sf::st_point(c(Buff.bbox["xmax"]+10000,
                           mean(Buff.bbox[c("ymin","ymax")])))}
  if(cut1Dir == "N"){
    cut1.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                           Buff.bbox["ymax"]+10000))}
  if(cut1Dir == "S"){
    cut1.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                           Buff.bbox["ymin"]-10000))}
  cut1.far <- cut1.far %>% sf::st_sfc(crs=26910)

  if(cut2Dir == "W"){
    cut2.far <- sf::st_point(c(Buff.bbox["xmin"]-10000,
                           mean(Buff.bbox[c("ymin","ymax")])))}
  if(cut2Dir == "E"){
    cut2.far <- sf::st_point(c(Buff.bbox["xmax"]+10000,
                           mean(Buff.bbox[c("ymin","ymax")])))}
  if(cut2Dir == "N"){
    cut2.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                           Buff.bbox["ymax"]+10000))}
  if(cut2Dir == "S"){
    cut2.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                           Buff.bbox["ymin"]-10000))}
  cut2.far <- cut2.far %>% sf::st_sfc(crs=26910)

  cuts <- c(sf::st_nearest_points(Buff.Line,cut1.far),
            sf::st_nearest_points(Buff.Line,cut2.far))

  Buff.Line.split <-
    lwgeom::st_split(Buff.Line,cuts) %>%
    sf::st_collection_extract(type = "LINESTRING")
  rightSide <- cut1 <- sf::st_union(Buff.Line.split[1],Buff.Line.split[3]) #RightSide
  leftSide <- cut2 <- Buff.Line.split[2] #LeftSide

  ls0 = sf::st_nearest_points(leftSide,sampledPoints) %>%
    data.frame() %>%
    dplyr::mutate(pointID = sampledPoints$pointID,
                  Side = "ls") %>%
    sf::st_as_sf()
  rs0 = sf::st_nearest_points(rightSide,sampledPoints) %>%
    data.frame() %>%
    dplyr::mutate(pointID = sampledPoints$pointID,
                  Side = "rs") %>%
    sf::st_as_sf()

  if(getSatImage & ggmap::has_google_key()){
    satImage <- ggmap::get_googlemap(center = c(mean(plotBbox.WGS[c(1,3)]),
                                                mean(plotBbox.WGS[c(2,4)])),
                                     zoom = googleZoom,
                                     maptype = "satellite")

  } else{
    cat(crayon::red("Sat image not retrieved.. getting stamenmap instead."))
    satImage <- ggmap::get_stamenmap(c(left = plotBbox.WGS[1] %>% as.numeric(),
                                       bottom = plotBbox.WGS[2] %>% as.numeric(),
                                       right = plotBbox.WGS[3] %>% as.numeric(),
                                       top = plotBbox.WGS[4]) %>% as.numeric(),zoom=15,maptype = "terrain")
   # satImage <- NULL
  }

  output <- list(mainLine = streamChannel,
                 leftSide = leftSide,
                 rightSide = rightSide,
                 ls0 = ls0,
                 rs0 = rs0,
                 plotBbox = plotBbox.WGS,
                 satImage = satImage,
                 sampledPoints = sampledPoints)
  return(output)
}
