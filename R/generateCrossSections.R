#' Generate Stream Cross Sections
#'
#' This is the first function to run when using the \code{ProcessSpace` package. It
#' generates a TransectObject object that is the passed to additional functions
#' to add additional details like cross section elevations and to generate
#' spatial exports.
#'
#' @param streamChannel  The target stream reach. This should be a single
#'   contiguous streamline. No forking or gaps.
#' @param getSatImage Logical (\strong{TRUE}). This determines whether to download
#'   satellite imagery. It requires a Google Authentication Key. See
#'   \code{ggmap::register_google()} to set up satellite imagery.
#' @param googleZoom Zoom level of satellite imagery. 15-17 typically work.
#' @param cut1Dir Important! Set this to the cardinal direction ("W","N","E","S")
#'  of the upstream end of the target streamline.
#' @param cut2Dir Depreciated. No longer used.
#' @param xSectionLength Length of cross sections away from the channel. Consider
#'  using \code{units::as_units(100,"m")} format.
#' @param xSectionDensity Gap space between cross sections.Consider
#'  using \code{units::as_units(20,"m")} format.
#'
#' @return transectObject.  This object will be passed onto additional function
#'  in `ProcessSpace`. Initially it is returned with just, streamline, boundary
#'   of transects, transect lines, a bounding box, the retreived google or
#'   Stamen map, and the points where the cross sections cross the streamline.
#' @export
#'
#' @examples
#'


generateCrossSections <- function(streamChannel,
                                  getSatImage=TRUE,
                                  googleZoom = 14,
                                  cut1Dir = "W",
                                  cut2Dir = "E", #Depreciated No longer used
                                  xSectionLength = units::as_units(100,"m"),
                                  xSectionDensity = units::as_units(100,"m")){

  ##ERROR CHECKS:
  if(!(cut1Dir %in% c("W","N","E","S"))){
    cat(crayon::red("cut1Dir not in c(\"W\",\"N\",\"E\",\"S\").
                      Defaulting to \"W\" which may impact left/right side labeling."))
    cut1Dir <- "W"
  }


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
  #In extra curvy streams with small segments, some segments gets reversed wrong:
  temp$doReverse <- median(temp$doReverse) %>% as.logical()

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

  ##I think the following *_small code was added to clean up the ends.
  ## But it introduced an error, causing some boundaries to be clipped in the middle.
  ## Commenting out for now.
  #
  # buff1_small <-  sf::st_difference(buff1,
  #                             y=sf::st_buffer(buff2,
  #                                             dist = as.numeric(xSectionLength*1.1),
  #                                             nQuadSegs = 100))
  # buff2_small <-  sf::st_difference(buff2,
  #                             y=sf::st_buffer(buff1,
  #                                             dist = as.numeric(xSectionLength*1.1),
  #                                             nQuadSegs = 100))
  #
  # buff1 <- buff1_small
  # buff2 <- buff2_small

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


  ##############
  sampledPoints <- sf::st_line_sample(streamChannel.union,
                                      density= xSectionDensity,
                                      type="regular") %>%
    sf::st_cast("POINT",warn=FALSE) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(pointID = rev(1:dplyr::n()))


  ##########

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


  plotBbox.WGS <- sf::st_union(leftSide,rightSide) %>%
    sf::st_transform(crs=4326) %>%
    sf::st_bbox()

  if(getSatImage & ggmap::has_google_key()){
    # if(is.null(googleZoom)){
    #   targetLength = sf::st_length(streamChannel) %>% sum()
    #   googleZoom = dplyr::case_when(##NEED TO TEST AND FILL THIS IN
    # )
    # }
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
