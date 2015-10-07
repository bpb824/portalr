#' Aggregate PORTAL data over time.
#'
#' @param unAgg Data that you want to aggregate.
#' @param aggVars Variables you want to retain in aggregation, such as 'lanenumber' or 'stationid'
#' @param timeCut A string indicating how you want to aggregate over time. Examples include '1 min', '5 min', '1 hour', '1 day', etc.
#' @param acrossDays A boolean indicating weather you want to aggregate by your selected time cut across days.
#'
#' @return Aggregated data.
#' @export
aggTime= function(unAgg, aggVars, timeCut, acrossDays = FALSE){
  unAgg$period = cut(unAgg$starttime,breaks =timeCut)
  agg = plyr::ddply(unAgg, c(aggVars,"period"),function(X) data.frame(volume=sum(X$volume,na.rm = TRUE), occupancy = mean(X$occupancy,na.rm = TRUE), speed = weighted.mean(X$speed,X$volume,na.rm = TRUE)))
  agg$period = as.POSIXct(agg$period)
  #agg$lanenumber = factor(agg$lanenumber)
  if(acrossDays){
    agg$time = strftime(as.POSIXct(agg$period), format="%H:%M:%S")
    agg_time = plyr::ddply(agg,c(aggVars,"time"),function(X) data.frame(volume=mean(X$volume,na.rm = TRUE), occupancy = mean(X$occupancy,na.rm = TRUE), speed = weighted.mean(X$speed,X$volume,na.rm = TRUE)))
    agg_time$time = as.POSIXct(agg_time$time,format ="%H:%M:%S" )
    return(agg_time)
  }else{
    return(agg)
  }
}
