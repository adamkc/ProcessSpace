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
  plotter <- function(df){
    filename <- file.path(dir,paste0("Transect_",df$Transect[1],"_temp_.png"))
    ggplot2::ggplot(df,ggplot2::aes(x=metersLength,y=deltaEl)) +
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
      # geom_area(data=df %>% dplyr::filter(deltaEl>0),
      #           aes(x=metersLength,y=deltaEl),fill="khaki3") +
      # geom_line(data=df %>% dplyr::filter(deltaEl>0),
      #           aes(x=metersLength,y=deltaEl),col="orange3") +
      # geom_area(data=df %>% dplyr::filter(deltaEl<0),
      #           aes(x=metersLength,y=deltaEl),fill="blue4",alpha=.3) +
      # geom_line(data=df %>% dplyr::filter(deltaEl<0),
      #           aes(x=metersLength,y=deltaEl),col="orange3") +
      ggplot2::geom_hline(yintercept = 0,linetype=3,size=.3) +
      ggplot2::geom_hline(yintercept = 0.3,linetype=2,alpha=.8,col="blue3",size=.3) +
      ggplot2::geom_hline(yintercept = 0.6,linetype=2,alpha=.5,col="green3",size=.3) +
      ggplot2::geom_vline(xintercept = 0,col="royalblue2",alpha=.5,size=.3) +
      ggplot2::theme_classic() +
      #coord_fixed(ratio=20) +
      ggplot2::ylab("Height above channel (m)") +
      ggplot2::xlab("Distance off channel (m)") +
      ggplot2::ggtitle(paste0("Transect_",df$Transect[1])) +
      ggplot2::geom_text(label="Channel",col="royalblue2",x=0,y=0,angle=90,
                         hjust=-.4,vjust=-.3,alpha=.5,size=2) +
      ggplot2::theme(text=ggplot2::element_text(size=4)) +
      ggplot2::xlim(min(df$metersLength),max(df$metersLength))
    ggplot2:: ggsave(filename,height=2,width=4)

  }

  temp <- transectObject$XSectionPlotData %>%
    data.frame()
  numPlots <- length(unique(temp$Transect))
  cat(crayon::yellow(sprintf("There are %s ggplots being generated",numPlots)))
  for(i in unique(temp$Transect)){
    temp %>% dplyr::filter(Transect==i) %>% plotter()
  }

}




#' kml_compress_fixed This function is based on the kml_compress from plotKML
#' but fixes a bug with including files
#'
#' @param file.name
#' @param zip
#' @param files
#' @param rm
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
kml_compress_fixed <- function(file.name, files = "", removeTemps = TRUE)
{
  extension <- tools::file_ext(file.name)
  kmz <- stringr::str_replace(file.name, extension, "kmz")

  zip::zipr(zipfile = kmz,files = files,recurse = TRUE)

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
