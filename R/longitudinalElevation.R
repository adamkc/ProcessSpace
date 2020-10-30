#' longitudinalElevation
#'
#' @param transectObject
#' @param rasterDir
#' @param makePlot
#' @param returnData
#' @param plotFileName
#'
#' @return
#' @export
#'
#' @examples
longitudinalElevation <- function(transectObject,
           rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
           returnPlots=TRUE,
           returnData=TRUE,
           plotFileName="longitudinalPlot.png"){
    cat(crayon::green("Generating Longitudinal Elevation\n"))

    r <- raster::raster(rasterDir)


    ElPoints <- transectObject$mainLine %>%
      sf::st_line_sample(density = units::as_units(1,"ft")) %>% #1pts/foot
      sf::st_cast("POINT") %>% sf::st_sf()

    suppressWarnings(ElPoints$El <- raster::extract(r,ElPoints))

    ElPoints <- ElPoints %>%
      tidyr::fill(El,.direction="downup") %>%
      dplyr::mutate(index=1:dplyr::n(),
                    El = round(El,2),
                    blip = ((dplyr::lag(El)<El|
                               dplyr::lag(dplyr::lag(El))<El|
                               dplyr::lag(dplyr::lag(dplyr::lag(El)))<El) &
                              (dplyr::lead(El)<El|
                                 dplyr::lead(dplyr::lead(El))|
                                 dplyr::lead(dplyr::lead(dplyr::lead(El)))))) %>%
      dplyr::filter(!(blip))%>%
      dplyr::mutate(rollEl = stats::filter(El, c(.2,.2,.2,.2,.2)),
                    segLength_m = sum(sf::st_length(transectObject$mainLine)),
                    slope = as.numeric((units::as_units(El-dplyr::lead(El),"m")/
                                          units::as_units(1,"ft"))),
                    lengthAlong = as.numeric((index/dplyr::n())*segLength_m %>%
                                               units::set_units("m")))

    # ElPoints %>%  ggplot2::ggplot(ggplot2::aes(x=lengthAlong,y=slope)) +
    #   ggplot2::geom_smooth() + ggplot2::geom_line(alpha=.2)
    #
    # ElPoints %>%
    #   ggplot2::ggplot(ggplot2::aes(x=lengthAlong,y=rollEl)) +
    #   ggplot2::geom_line()


    if(returnPlots==TRUE){
      longitudinalPlotData <- ElPoints %>%
        tidyr::drop_na() %>%
        dplyr::mutate(percentLength = seq(from=0,to=1,length.out = dplyr::n()),
                      metersLength = percentLength*segLength_m,
                      deltaLength = segLength_m/dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cumulativeLength=as.numeric(cumsum(deltaLength)))

      breaks = (max(longitudinalPlotData$cumulativeLength)/5) %>%
        round(-1)*c(0:5)

      cumPlot1 <- ggplot2::ggplot(longitudinalPlotData,
                                  ggplot2::aes(x=cumulativeLength,y=El)) +
        ggplot2::geom_line(size=1,show.legend=FALSE)  +
        ggplot2::scale_color_gradient(low = "blue",high="green") +
        ggplot2::ylab("Elevation (m)") +
        ggplot2::scale_x_continuous(breaks = breaks)  +
        ggplot2::ggtitle("Longitudinal Profile") +
        ggplot2::theme_bw()+
        ggplot2::theme(axis.title.x = ggplot2::element_blank())

      segmentLength = round(max(longitudinalPlotData$cumulativeLength)/40, -1)
      cumPlot2 <- longitudinalPlotData %>%
        dplyr::mutate(cumulativeLengthSegment = cumulativeLength +
                        (segmentLength - cumulativeLength %% segmentLength),
                      pixelSlope = El-dplyr::lag(El)) %>%
        dplyr::group_by(cumulativeLengthSegment) %>%
        dplyr::summarize(segmentEl = mean(El),
                         segmentLength=  dplyr::last(cumulativeLength)-dplyr::first(cumulativeLength),
                         segmentSlopeperPixel = mean(pixelSlope),
                         segmentPercentSlope = (sum(pixelSlope)/segmentLength)*-100) %>%
        dplyr::mutate(TenMeterSlope = segmentEl-dplyr::lag(segmentEl)) %>%
        ggplot2::ggplot(ggplot2::aes(x=cumulativeLengthSegment,y=segmentPercentSlope)) +
        ggplot2::geom_line(ggplot2::aes(col=as.numeric(cumulativeLengthSegment)),
                           show.legend=FALSE) +
        ggplot2::geom_smooth() +
        ggplot2::scale_color_gradient(low = "blue",high="green") +
        ggplot2::ylab("Percent Slope") +
        ggplot2::xlab("Cumulative Length (m)") +
        ggplot2::geom_text(label=sprintf("Segment Length: %s m",segmentLength),
                           x=0,y=0,hjust=-0,size=3) +
        ggplot2::scale_x_continuous(breaks=breaks) +
        #scale_y_continuous(breaks = c(0,-2,-4,-6),
        #minor_breaks = c(-1,-3,-5))+
        ggplot2::theme_bw()#;cumPlot2
      transectObject$longProGraph1 <- cumPlot1
      transectObject$longProGraph2 <- cumPlot2
    }

    if(returnData == TRUE){
      transectObject$longitudinalPlotData <- longitudinalPlotData
    }

    if(returnData | returnPlots){
      return(transectObject)
    }

  }
