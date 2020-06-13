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
                                  xSectionLength = units::as_units(100,"m"),
                                  xSectionDensity = units::as_units(100,"m")){

  #streamChannel <- sf::st_transform(streamChannel, sf::st_crs(streamChannel))

  plotBbox.WGS <- sf::st_bbox(streamChannel %>%
                                sf::st_transform(crs=4326))

  streamChannel.union <- sf::st_union(streamChannel) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_line_merge()%>%
    sf::st_cast("LINESTRING",warn=FALSE) %>%
    sf::st_sfc() %>%
    sf::st_transform(crs=sf::st_crs(streamChannel))
  #streamChannel.union$Shape_Leng = st_length(streamChannel.union)

  channelLength = sum(sf::st_length(streamChannel.union))

  ####

  buff1 <- sf::st_buffer(streamChannel %>% sf::st_union(),
                         dist = as.numeric(xSectionLength),
                         nQuadSegs = 100,
                         singleSide=TRUE) %>%
    smoothr::smooth("ksmooth",smoothness=20) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_difference(y=sf::st_buffer(streamChannel %>% sf::st_union(),
                                      dist = as.numeric(xSectionLength*0.95),
                                      nQuadSegs = 100)) %>%
    sf::st_cast("MULTILINESTRING") %>%
    sf::st_transform(crs=sf::st_crs(streamChannel))


  buff2 <- sf::st_buffer(streamChannel %>% sf::st_union(),
                         dist = as.numeric(-xSectionLength),
                         nQuadSegs = 100,
                         singleSide=TRUE)%>%
    smoothr::smooth("ksmooth",smoothness=20) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_line_merge() %>%
    sf::st_difference(y=sf::st_buffer(streamChannel %>% sf::st_union(),
                                      dist = as.numeric(xSectionLength*0.95),
                                      nQuadSegs = 100))%>%
    sf::st_cast("MULTILINESTRING") %>%
    sf::st_transform(crs=sf::st_crs(streamChannel))

  leftSide <- buff1
  rightSide <- buff2


  buff1.center <- sf::st_centroid(buff1) %>% sf::st_coordinates()
  buff2.center <- sf::st_centroid(buff2) %>% sf::st_coordinates()
  Buff.bbox <- sf::st_bbox(streamChannel)

  if(cut1Dir == "W"){
    cut1.far <- sf::st_point(c(Buff.bbox["xmin"]-10000,
                               mean(Buff.bbox[c("ymin","ymax")])))
    if(buff1.center[2]>buff2.center[2]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }
  if(cut1Dir == "E"){
    cut1.far <- sf::st_point(c(Buff.bbox["xmax"]+10000,
                               mean(Buff.bbox[c("ymin","ymax")])))
    if(buff1.center[2]<buff2.center[2]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }
  if(cut1Dir == "N"){
    cut1.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                               Buff.bbox["ymax"]+10000))
    if(buff1.center[1]>buff2.center[1]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }
  if(cut1Dir == "S"){
    cut1.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                               Buff.bbox["ymin"]-10000))
    if(buff1.center[1]<buff2.center[1]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }

  cut1.far <- cut1.far %>% sf::st_sfc(crs=sf::st_crs(streamChannel))
  #
  #   Buff.Line <-
  #     sf::st_buffer(streamChannel %>% sf::st_union(),
  #                   dist = xSectionLength,nQuadSegs = 100) %>%
  #     smoothr::smooth("ksmooth",smoothness=20) %>%
  #     sf::st_cast("MULTILINESTRING",warn=FALSE)
  #
  #   if(cut2Dir == "W"){
  #     cut2.far <- sf::st_point(c(Buff.bbox["xmin"]-10000,
  #                            mean(Buff.bbox[c("ymin","ymax")])))}
  #   if(cut2Dir == "E"){
  #     cut2.far <- sf::st_point(c(Buff.bbox["xmax"]+10000,
  #                            mean(Buff.bbox[c("ymin","ymax")])))}
  #   if(cut2Dir == "N"){
  #     cut2.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
  #                            Buff.bbox["ymax"]+10000))}
  #   if(cut2Dir == "S"){
  #     cut2.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
  #                            Buff.bbox["ymin"]-10000))}
  #   cut2.far <- cut2.far %>% sf::st_sfc(crs=raster::crs(streamChannel))
  #
  #   cuts <- c(sf::st_nearest_points(Buff.Line,cut1.far),
  #             sf::st_nearest_points(Buff.Line,cut2.far))
  #
  #   Buff.Line.split <-
  #     lwgeom::st_split(Buff.Line,cuts) %>%
  #     sf::st_collection_extract(type = "LINESTRING")
  #   rightSide <- cut1 <- sf::st_union(Buff.Line.split[1],Buff.Line.split[3]) #RightSide
  #   leftSide <- cut2 <- Buff.Line.split[2] #LeftSide

  ##############
  sampledPoints <- sf::st_line_sample(streamChannel.union,
                                      density= xSectionDensity,
                                      type="regular") %>%
    sf::st_cast("POINT",warn=FALSE) %>% sf::st_as_sf() %>%
    dplyr::mutate(dist = sf::st_distance(.,cut1.far),
                  test = dplyr::first(dist)>dplyr::last(dist),
                  pointID = ifelse(test,
                                   1:dplyr::n(),
                                   rev(1:dplyr::n()))) %>%
    dplyr::select(-test,-dist)

  # sampledPoints_ls <- sf::st_line_sample(leftSide %>%
  #                                          sf::st_cast("LINESTRING",
  #                                                      warn=FALSE),
  #                                        n= nrow(sampledPoints),
  #                                        type="regular") %>%
  #   sf::st_cast("POINT",warn=FALSE) %>% st_as_sf() %>%
  #   dplyr::mutate(dist = st_distance(.,cut1.far),
  #                 test = first(dist)>last(dist),
  #                 pointID = ifelse(test,
  #                                  1:dplyr::n(),
  #                                  rev(1:dplyr::n()))) %>%
  #   select(-test,-dist)
  #
  # sampledPoints_rs <- sf::st_line_sample(rightSide %>% st_union() %>%
  #                                          sf::st_cast("LINESTRING",
  #                                                      warn=FALSE),
  #                                        n= nrow(sampledPoints),
  #                                        type="regular") %>%
  #   sf::st_cast("POINT",warn=FALSE) %>% st_as_sf() %>%
  #   dplyr::mutate(dist = st_distance(.,cut1.far),
  #                 test = first(dist)>last(dist),
  #                 pointID = ifelse(test,
  #                                  1:dplyr::n(),
  #                                  rev(1:dplyr::n()))) %>%
  #   select(-test,-dist)
  ##########

  ls0 = sf::st_nearest_points(leftSide,sampledPoints) %>%
    data.frame() %>%
    dplyr::mutate(pointID = sampledPoints$pointID,
                  Side = "ls") %>%
    sf::st_as_sf()

  # ls1 <- lapply(X = 1:nrow(sampledPoints), FUN = function(i) {
  #   pair <- rbind(sampledPoints, sampledPoints_ls) %>%
  #     dplyr::filter(pointID == i) %>% st_combine()
  #   line <- st_cast(pair, "LINESTRING") %>%
  #     st_as_sf() %>% mutate(pointID = i)
  #   return(line)
  # }) %>% do.call("rbind", .)


  rs0 = sf::st_nearest_points(rightSide,sampledPoints) %>%
    data.frame() %>%
    dplyr::mutate(pointID = sampledPoints$pointID,
                  Side = "rs") %>%
    sf::st_as_sf()

  # rs1 <- lapply(X = 1:nrow(sampledPoints), FUN = function(i) {
  #   pair <- rbind(sampledPoints, sampledPoints_rs) %>%
  #     dplyr::filter(pointID == i) %>% st_combine()
  #   line <- st_cast(pair, "LINESTRING") %>%
  #     st_as_sf() %>% mutate(pointID = i)
  #   return(line)
  #   }) %>% do.call("rbind", .)

  if(getSatImage & ggmap::has_google_key()){
    satImage <- ggmap::get_googlemap(center = c(mean(plotBbox.WGS[c(1,3)]),
                                                mean(plotBbox.WGS[c(2,4)])),
                                     zoom = googleZoom,
                                     maptype = "satellite")

  } else{
    cat(crayon::red("Sat image not retrieved.. getting stamenmap instead.\n See ggmap::register_google() to set up satellite imagery."))
    satImage <- ggmap::get_stamenmap(c(left = plotBbox.WGS[1] %>% as.numeric(),
                                       bottom = plotBbox.WGS[2] %>% as.numeric(),
                                       right = plotBbox.WGS[3] %>% as.numeric(),
                                       top = plotBbox.WGS[4]) %>% as.numeric(),
                                     zoom=15,maptype = "terrain")
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
