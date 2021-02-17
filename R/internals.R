utils::globalVariables(c(".", "Area_m2", "DetrendedElevation", "Dist",
                         "DistLS", "DistRS", "Edge", "EdgeTF", "El",
                         "Elevation", "Index", "Name", "SAGA_pal",
                         "Segment", "Side", "Side.1", "Transect", "Value",
                         "cumulativeLength", "cumulativeLengthSegment",
                         "deltaEl", "deltaElCat",
                         "deltaLength", "exceedsLimit", "geometry",
                         "label", "metersLength", "optional",
                         "pastFirstLimit", "percentLength", "pixelSlope",
                         "pointID", "pointID.1", "segLength_m",
                         "segmentEl", "segmentPercentSlope", "streamEl",
                         "test", "vegEdge", "waterEdge", "x", "y"))


#' extent.sf
#'
#' @param x
#'
#' @return
#' @noRd
#'
#' @examples
#'
extent.sf <- function(x) {
  raster::extent(unclass(sf::st_bbox(x))[c("xmin", "xmax", "ymin", "ymax")])
}



#' outputTimer
#'
#' @param startTime
#'
#' @return
#' @noRd
#'
#' @examples
outputTimer <- function(startTime){
  runTime = difftime(Sys.time(), startTime,units="mins") %>%
    round(2) %>% format()
  cat(crayon::yellow(paste0(" -- Completed in ", runTime, ".\n")))
}

#' exportIndividualXSectionPlots
#'
#' @param transectObject
#' @param sectionName
#'
#' @return
#' @noRd
#'
#' @examples

exportIndividualXSectionPlots <- function(transectObject,sectionName){
  dir <- paste0(sectionName,"-Images")
  if(!dir.exists(dir)) dir.create(dir)
  ###
  insetPlotter <- function(i){
    ggplot() +
      geom_sf(data=transectObject$mainLine,
              col="blue2") +
      geom_sf(data=transectObject$sampledPoints,
              col="black",size=0.6) +
      geom_sf(data=transectObject$sampledPoints %>%
                dplyr::filter(pointID==i),
              col="red2",size=1.5) +
      geom_sf(data=transectObject$ls0 %>%
                dplyr::filter(pointID==i),
              col="green2") +
      geom_sf(data=transectObject$rs0 %>%
                dplyr::filter(pointID==i),
              col="green2") +
      theme_nothing() +
      theme(panel.border = element_rect(fill = NA)) +
      coord_sf()
  }
  ###
  plotter <- function(df,insetPlot=NULL){
    filename <- file.path(dir,paste0("Transect_",df$Transect[1],"_temp_.png"))
    plot <- ggplot2::ggplot(df,ggplot2::aes(x=metersLength,y=deltaEl)) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = 0, ymax = 0.3,fill="blue3",
                        alpha = .3, color = NA) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = .3, ymax = 0.6,fill="green3",
                        alpha = .3, color = NA) +
      ggplot2::geom_area(data=df,
                         ggplot2::aes(x=metersLength,y=deltaEl),fill="khaki3") +
      ggplot2::geom_line(data=df,
                         ggplot2::aes(x=metersLength,y=deltaEl),col="orange3") +
      ggplot2::geom_hline(yintercept = 0,linetype=3,size=.3) +
      ggplot2::geom_hline(yintercept = 0.3,linetype=2,alpha=.8,col="blue3",size=.3) +
      ggplot2::geom_hline(yintercept = 0.6,linetype=2,alpha=.5,col="green3",size=.3) +
      ggplot2::geom_vline(xintercept = 0,col="royalblue2",alpha=.5,size=.3) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Height above channel (m)") +
      ggplot2::xlab("Distance off channel (m)") +
      ggplot2::ggtitle(paste0("Transect_",df$Transect[1])) +
      ggplot2::geom_text(label="Channel",col="royalblue2",x=0,y=0,angle=90,
                         hjust=-.4,vjust=-.3,alpha=.5,size=2) +
      ggplot2::theme(text=ggplot2::element_text(size=4)) +
      ggplot2::xlim(min(df$metersLength),max(df$metersLength))
    if(is.null(insetPlot))
    {
      ggplot2::ggsave(filename = filename,plot = plot,height=2,width=4,dpi=150)
    } else
    {
      gg_all <- cowplot::ggdraw() +
        cowplot::draw_plot(plot) +
        cowplot::draw_plot(insetPlot,x=0.7,y=0.7,width=.3,height=.3)
      ggplot2::ggsave(filename = filename,plot = gg_all,height=2,width=4,dpi=150)
    }

  }

  temp <- transectObject$XSectionPlotData %>%
    data.frame()
  numPlots <- length(unique(temp$Transect))
  cat(crayon::yellow(sprintf("There are %s ggplots being generated.\n",numPlots)))
  for(i in unique(temp$Transect)){
    cat(paste0(i,", "))
    inset <- insetPlotter(i)
    temp %>% dplyr::filter(Transect==i) %>% plotter(insetPlot = inset)
  }
}




#' kml_compress_fixed This function is based on the kml_compress from plotKML
#' but fixes a bug with including files
#'
#' @param file.name
#' @param files
#' @param removeTemps
#'
#' @return
#' @export
#'
#' @examples
kml_compress_fixed <- function(file.name, files = "", removeTemps = TRUE)
{
  extension <- tools::file_ext(file.name)
  kmz <- gsub(x = file.name,
              pattern =  extension,
              replacement =  "kmz")

  if (any(!file.exists(files)))
    files <- files[file.exists(files)]

  #zip::zip is depreciated and may not work on all systems. But by default it
  #handles folders passed in file.name and files arguments. So We'll run with it
  #until it doenst work anymore....
  zip::zip(zipfile = kmz,
           files = files,
           recurse = TRUE,
           include_directories = TRUE)

  #
  #
  # if (.Platform$OS.type == "windows") {
  #   suppressMessages( try(x <- zip(zipfile = kmz,
  #                                  files = c(file.name, files),
  #                                  flags="-r9Xq",
  #                                  zip = zipCommand)))
  # }
  # else {
  #   suppressMessages( try(x <- zip(zipfile = kmz, files = file.name,
  #                                  flags="-r9Xq", zip = zipCommand)))
  # }
  # if(x == 127){
  #   warning("KMZ generation failed. Error 127: unable to locate zip utility")
  # }
  # if (methods::is(.Last.value, "try-error")) {
  #   if (zipCommand == "" | !nzchar(zipCommand)) {
  #     warning("KMZ generation failed. No zip utility has been found.")
  #   }
  #   else {
  #     warning("KMZ generation failed. Wrong command passed to 'zipCommand = ... option'.")
  #   }
  # }
  if (file.exists(kmz) & removeTemps == TRUE) {
    x <- file.remove(file.name, files)
  }
}


#' concavemanWrapper
#'
#' @param multilinestring
#'
#' @return
#' @noRd
#'
#' @examples
concavemanWrapper <- function(multilinestring){
  temp <- multilinestring %>% #st_combine() %>%
    #st_cast("LINESTRING",warn=FALSE) %>% st_sf() %>%
    dplyr::arrange(Side,pointID) %>%
    dplyr::mutate(set1 = ceiling(dplyr::row_number()/2),
                  set2 = floor(dplyr::row_number()/2)) %>%
    sf::st_cast("POINT",warn=FALSE)
  ##These four calls to purrr are the only place purrr is used in ProcessSpace (As of Feb 2021). Remove asap.
  set1.polys <- purrr::map(unique(temp$set1),
                           ~ concaveman::concaveman(temp[temp$set1 %in% .,])) %>%
    purrr::reduce(rbind) %>%
    dplyr::mutate(polygonID=paste0("1.",unique(temp$set1)),
                  pointID = as.numeric(unique(temp$set1))*2,
                  Area_m2 = sf::st_area(.))

  set2.polys <- purrr::map(unique(temp$set2),
                           ~ concaveman::concaveman(temp[temp$set2 %in% .,])) %>%
    purrr::reduce(rbind) %>%
    dplyr::mutate(polygonID=paste0("2.",unique(temp$set2)),
                  pointID = (as.numeric(unique(temp$set2))*2)+1,
                  Area_m2 = sf::st_area(.))


  #Drop the single feature "polygon":
  set1.polys <- dplyr::filter(set1.polys, sf::st_is_valid(set1.polys))
  set2.polys <- dplyr::filter(set2.polys, sf::st_is_valid(set2.polys))

  #fullPolygon <- st_union(set1.polys,set2.polys) %>% st_union()
  fullPolygons <- rbind(set1.polys,set2.polys)
  return(fullPolygons)
}





