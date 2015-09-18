#' freewaySpeedMap
#'
#' Produces a speed-based heatmap (AKA brainscan plot) for a corridor with PORTAL's freeway data system. See Freeway Speed Map vignette for example usage.
#'
#' @param con database connection PORTAL PostgreSQL database
#' @param corridorID ID number of the corridor to plot. See 'corrdidors' table to select ID for plotting.
#' @param startDate Start date of data to query (YYYY-MM-DD format)
#' @param endDate End date of data to query (YYYY-MM-DD format)
#' @param weekdays Boolean indicating whether to subset data to weekdays. Defaults to TRUE.
#' @param outputPng .png file path of output plot. Default is NULL; if NULL plots to current device (i.e. RStudio plot device).
#'
#' @return None
#' @export
freewaySpeedMap = function(con,corridorID,startDate,endDate,weekdays=TRUE,outputPng = NULL){

  print("Querying PORTAL database...")
  corridors  = DBI::dbGetQuery(con,"SELECT * FROM public.corridors")
  corridor_stations = DBI::dbGetQuery(con,"SELECT * FROM public.corridorstations")
  stations= DBI::dbGetQuery(con,"SELECT * FROM public.stations")
  detectors = DBI::dbGetQuery(con,"SELECT * FROM public.detectors")

  corStations = subset(corridor_stations,corridorid==corridorID)
  mainline = corStations$stationid[corStations$stationid<5000]
  dets = detectors$detectorid[detectors$stationid %in% mainline]
  raw = freewayData(con,dets,startDate,endDate)
  print("Finished query, aggregating data...")

  if(weekdays){
    raw$dow = weekdays(as.POSIXct(raw$starttime))
    raw = subset(raw,raw$dow != "Saturday" & raw$dow != "Sunday")
  }

  raw = plyr::join(raw,detectors, by="detectorid")
  raw = plyr::join(raw,stations, by ="stationid", match="first")

  agg = aggTime(raw,c("milepost","stationid"),"5 min",acrossDays = TRUE)



  agg$hour = lubridate::hour(agg$time)+lubridate::minute(agg$time)/60

  #plotData = aggData[,c("hour","milepost","speed","stationid")]
  agg = subset(agg,!is.nan(agg$speed))
  #colnames(plotData) =c("x","y","z")
  plotInterp <- akima::interp2xyz(akima::interp(agg$hour,agg$milepost,agg$speed,xo=seq(min(agg$hour), max(agg$hour), length = 288),duplicate = "mean"), data.frame = TRUE)
  stationRef= stations[stations$stationid %in% agg$stationid & is.na(stations$end_date),c("stationid","milepost","locationtext")]
  stationRef = stationRef[order(stationRef$milepost),]
  stationLabs = stationRef$locationtext
  for (i in 2:nrow(stationRef)){
    if((stationRef$milepost[i]-stationRef$milepost[i-1])<0.2){
        stationLabs[i] = ""
    }
  }

  hourTicks = c(0,4,8,12,16,20,24)
  hourLabs = c("12:00 AM", "4:00 AM", "8:00 AM", "12:00 PM", "4:00 PM", "8:00 PM", "12:00 AM")

  weekdayString = "for weekdays during "
  if(!weekdays){
    weekdayString = "for all days during "
  }

  plt = ggplot2::ggplot(plotInterp) +
    ggplot2::aes(x = x, y = y, z = z, fill = z) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_distiller(palette="RdYlGn", na.value="white",name="Speed (mph)") +
    ggplot2::scale_y_continuous(breaks = stationRef$milepost,labels =stationLabs)+
    ggplot2::scale_x_continuous(breaks= hourTicks,labels = hourLabs)+
    ggplot2::xlab("")+ggplot2::ylab("Location")+
    ggplot2::ggtitle(paste0("Speed heatmap for ", corridors$longname[corridors$corridorid==corridorID],
                   "\n",weekdayString, startDate, " to ", endDate))

  if(is.null(outputPng)){
    ggplot2::theme_set(ggplot2::theme_bw(base_size = 12))
    print(plt)
  }else{
    ggplot2::theme_set(ggplot2::theme_bw(base_size = 25))
    png(outputPng,width = 1500, height = 1000)
    print(plt)
    dev.off()
  }

  print("Speed heatmap plotted to device.")

}
