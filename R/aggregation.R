#' Aggregate PORTAL data over time.
#'
#' @param unAgg Data that you want to aggregate.
#' @param aggVars Variables you want to retain in aggregation, such as 'lanenumber' or 'stationid'
#' @param timeCut A string indicating how you want to aggregate over time. Examples include '1 min', '5 min', '1 hour', '1 day', etc.
#' @param hod A boolean indicating weather you want to aggregate by hour of day across multiple days.
#'
#' @return Aggregated data.
#' @export
aggTime= function(unAgg, aggVars, timeCut, hod = FALSE){
  unAgg$period = cut(unAgg$starttime,breaks =timeCut)
  agg = plyr::ddply(unAgg, c(aggVars,"period"),function(X) data.frame(volume=sum(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
  agg$period = as.POSIXct(agg$period)
  agg$lanenumber = factor(agg$lanenumber)
  if(hod){
    agg$hod = lubridate::hour(agg$period)
    agg_hour = plyr::ddply(agg,c(aggVars,"hod"),function(X) data.frame(volume=mean(X$volume), occupancy = mean(X$occupancy), speed = weighted.mean(X$speed,X$volume)))
    return(agg_hour)
  }else{
    return(agg)
  }
}
