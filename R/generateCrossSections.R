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
                                  #cut2Dir = "E", #Depreciated No longer used
                                  xSectionLength = units::as_units(100,"m"),
                                  xSectionDensity = units::as_units(100,"m")){


  cat(crayon::green("Realigning stream upstream to downstream."))
  startTime <- Sys.time()

  Buff.bbox <- sf::st_bbox(streamChannel)

  if(cut1Dir == "W"){
    cut1.far <- sf::st_point(c(Buff.bbox["xmin"]-10000,
                               mean(Buff.bbox[c("ymin","ymax")]))) %>%
      sf::st_sfc(crs=sf::st_crs(streamChannel))
  }
  if(cut1Dir == "E"){
    cut1.far <- sf::st_point(c(Buff.bbox["xmax"]+10000,
                               mean(Buff.bbox[c("ymin","ymax")]))) %>%
      sf::st_sfc(crs=sf::st_crs(streamChannel))
  }
  if(cut1Dir == "N"){
    cut1.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                               Buff.bbox["ymax"]+10000))  %>%
      sf::st_sfc(crs=sf::st_crs(streamChannel))
  }
  if(cut1Dir == "S"){
    cut1.far <- sf::st_point(c(mean(Buff.bbox[c("xmin","xmax")]),
                               Buff.bbox["ymin"]-10000)) %>%
      sf::st_sfc(crs=sf::st_crs(streamChannel))
  }


  ##OLD STREAM REALIGNMENT---------
  #
  # NVerts = dim(streamChannel%>%
  #                sf::st_cast("POINT",warn=FALSE))[1]
  #
  # streamPts <-  streamChannel  %>%
  #
  #   sf::st_line_sample(n=NVerts)%>%
  #   sf::st_combine() %>%
  #   sf::st_cast("POINT") %>%
  #   sf::st_as_sf() %>%
  #   #dplyr::select(geometry) %>%
  #   #dplyr::mutate(dist = sf::st_distance(.,cut1.far)) %>%
  #   dplyr::arrange(sf::st_distance(.,cut1.far)) #%>%
  # #dplyr::select(-dist)
  #
  # #streamPts$distFromFar = 1:nrow(streamPts)
  # distMat <- sf::st_distance(streamPts) %>%
  #   as.numeric() %>%
  #   matrix(nrow=nrow(streamPts))
  # #temp <- distMat
  # newOrder <- 1
  # pointJump <- rep(0,nrow(streamPts))
  #
  # for(i in 1:nrow(streamPts)) {
  #   thisPoint <- which(newOrder==i)[1]
  #   distMat[thisPoint,] <- NA
  #   nextPoint <- which.min(distMat[,thisPoint])
  #   newOrder[nextPoint] <- i+1
  #   if(i != nrow(streamPts))
  #      pointJump[i] <- distMat[nextPoint,thisPoint]
  # }
  #
  # streamPts$distFromTop <- newOrder
  #
  # streamPts <- streamPts[pointJump < (mean(pointJump)*5),]
  #
  # streamChannel.union <- streamPts %>%
  #   dplyr::arrange(distFromTop) %>%
  #   sf::st_combine() %>%
  #   sf::st_cast("LINESTRING") %>%
  #   sf::st_sf()
    # sf::st_coordinates() %>%
    # sf::st_linestring() %>%
    # sf::st_sfc(crs=sf::st_crs(streamChannel))

  temp <- streamChannel %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    dplyr::group_by(L1) %>%
    dplyr::summarize(X = c(dplyr::first(X),dplyr::last(X)),
                     Y = c(dplyr::first(Y),dplyr::last(Y)),
                     Place = c("first","last"),.groups="drop") %>%
    sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(streamChannel)) %>%
    dplyr::mutate(distance = sf::st_distance(.,cut1.far,by_element=TRUE)) %>%
    dplyr::group_by(L1) %>%
    dplyr::summarize(doReverse = dplyr::first(distance)>
                       dplyr::last(distance),
                     meanDist = as.numeric(mean(distance)),.groups="drop") %>%
    data.frame() %>% dplyr::select(L1,doReverse,meanDist)

  streamChannel.union <-streamChannel %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    dplyr::left_join(temp,by="L1") %>%
    dplyr::arrange(meanDist) %>%
    dplyr::group_by(L1) %>%
    dplyr::mutate(X=dplyr::case_when(doReverse==TRUE ~ rev(X),
                                     TRUE ~ X),
                  Y=dplyr::case_when(doReverse==TRUE ~ rev(Y),
                                     TRUE ~ Y)) %>%
    sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(streamChannel)) %>%
    sf::st_sf() %>%
    sf::st_combine() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_sf()

  # streamChannel.union <- sf::st_union(streamChannel) %>%
  #   sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
  #   sf::st_line_merge()%>%
  #   sf::st_cast("LINESTRING",warn=FALSE) %>%
  #   sf::st_sfc() %>%
  #   sf::st_transform(crs=sf::st_crs(streamChannel))
  # #streamChannel.union$Shape_Leng = st_length(streamChannel.union)
  #channelLength = sum(sf::st_length(streamChannel.union))

  ####
  ###TODO
  ### Error here, dist = as.numeric() added 4 times below because error message:
  #       Error in Ops.units(dist, 0) :
  #           both operands of the expression should be "units" objects
  ## Not sure what the fix is. So xSectionLength is measured in the units of the CRS
  ## of othe streamChannel.union object.
  buff1 <- sf::st_buffer(streamChannel.union,
                         dist = as.numeric(xSectionLength),
                         nQuadSegs = 100,
                         singleSide=TRUE) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_difference(y=sf::st_buffer(streamChannel.union,
                                      dist = as.numeric(xSectionLength*0.98),
                                      nQuadSegs = 100)) %>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_transform(crs=sf::st_crs(streamChannel))%>%
    smoothr::smooth("ksmooth",smoothness=20)   #One of two calls to smoothr. Maybe seek alternatives?


  buff2 <- sf::st_buffer(streamChannel.union,
                         dist = as.numeric(-xSectionLength),
                         nQuadSegs = 100,
                         singleSide=TRUE)%>%

    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_line_merge() %>%
    sf::st_difference(y=sf::st_buffer(streamChannel.union,
                                      dist = as.numeric(xSectionLength*0.98),
                                      nQuadSegs = 100))%>%
    sf::st_cast("MULTILINESTRING",warn=FALSE) %>%
    sf::st_transform(crs=sf::st_crs(streamChannel)) %>%
    smoothr::smooth("ksmooth",smoothness=20) #One of two calls to smoothr. Maybe seek alternatives?

  buff1_small <-  sf::st_difference(buff1,
                              y=sf::st_buffer(buff2,
                                              dist = as.numeric(xSectionLength*1.4),
                                              nQuadSegs = 100))
  buff2_small <-  sf::st_difference(buff2,
                              y=sf::st_buffer(buff1,
                                              dist = as.numeric(xSectionLength*1.4),
                                              nQuadSegs = 100))

  buff1 <- buff1_small
  buff2 <- buff2_small

  buff1.center <- sf::st_centroid(buff1) %>% sf::st_coordinates()
  buff2.center <- sf::st_centroid(buff2) %>% sf::st_coordinates()


  if(cut1Dir == "W"){
    if(buff1.center[2]>buff2.center[2]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }
  if(cut1Dir == "E"){
    if(buff1.center[2]<buff2.center[2]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }
  if(cut1Dir == "N"){
    if(buff1.center[1]>buff2.center[1]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }
  if(cut1Dir == "S"){
    if(buff1.center[1]<buff2.center[1]){
      rightSide <- buff1
      leftSide <- buff2
    }else{
      rightSide <- buff2
      leftSide <- buff1
    }
  }

  outputTimer(startTime)


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
  #     lwgeom::st_split(Buff.Line,cuts) %>% #removed lwgeom from DESCRIPTION FILE since this line is commented out.
  #     sf::st_collection_extract(type = "LINESTRING")
  #   rightSide <- cut1 <- sf::st_union(Buff.Line.split[1],Buff.Line.split[3]) #RightSide
  #   leftSide <- cut2 <- Buff.Line.split[2] #LeftSide

  ##############
  sampledPoints <- sf::st_line_sample(streamChannel.union,
                                      density= xSectionDensity,
                                      type="regular") %>%
    sf::st_cast("POINT",warn=FALSE) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(pointID = rev(1:dplyr::n()))


    # dplyr::mutate(dist = sf::st_distance(.,cut1.far),
    #               test = dplyr::first(dist)>dplyr::last(dist),
    #               pointID = ifelse(test,
    #                                1:dplyr::n(),
    #                                rev(1:dplyr::n()))) %>%
    # dplyr::select(-test,-dist)

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

  plotBbox.WGS <- sf::st_union(leftSide,rightSide) %>%
    sf::st_transform(crs=4326) %>%
    sf::st_bbox()

  if(getSatImage & ggmap::has_google_key()){
    satImage <- ggmap::get_googlemap(center = c(mean(plotBbox.WGS[c(1,3)]),
                                                mean(plotBbox.WGS[c(2,4)])),
                                     zoom = googleZoom,
                                     maptype = "satellite")

  } else{
    cat(crayon::red("Sat image not retrieved.. getting stamenmap instead.\n See ggmap::register_google() to set up satellite imagery.\n"))

    satImage <- ggmap::get_stamenmap(c(left = plotBbox.WGS[1] %>% as.numeric(),
                                       bottom = plotBbox.WGS[2] %>% as.numeric(),
                                       right = plotBbox.WGS[3] %>% as.numeric(),
                                       top = plotBbox.WGS[4]) %>% as.numeric(),
                                     zoom=15,maptype = "terrain")
    # satImage <- NULL
  }

  output <- list(mainLine = streamChannel.union,
                 leftSide = leftSide,
                 rightSide = rightSide,
                 ls0 = ls0,
                 rs0 = rs0,
                 plotBbox = plotBbox.WGS,
                 satImage = satImage,
                 sampledPoints = sampledPoints)
  return(output)
}
