#' Filter freeway data using threshold values
#'
#' This function will use six criteria developed for filtering PORTAL data to filter your raw data. The criteria are summmarized on Table 1 of this document: http://portal.its.pdx.edu/Portal/static/files/fhwa/Freeway%20Data%20Documentation.pdf
#'
#' @param fdata Your raw freeway data.
#'
#' @return Filtered freeway data
#' @export
#'
filterFreeway = function(fdata){

  dets = unique(fdata$detectorid)
  badRows = vector()
  for (i in 1:length(dets)){
    did =dets[i]
    sid = detectors$stationid[detectors$detectorid==did]
    detData = subset(fdata,fdata$detectorid==did)
    if(sid>=5000){
      bad = c(which(detData$volume>17))
      bad = unique(bad)
      badRows = c(badRows,rownames(detData[bad,]))
    }else{
      bad = c(which(detData$volume>17),
              which(detData$occupancy>95),
              which(detData$speed>100),
              which(detData$speed<5),
              which(detData$speed==0 & detData$volume>0),
              which(detData$speed>0 & detData$volume==0),
              which(detData$occupancy>0 & detData$volume==0))
      bad = unique(bad)
      badRows = c(badRows,rownames(detData[bad,]))
    }
  }
  badRows = unique(badRows)
  goodRows = rownames(fdata)[!(rownames(fdata) %in% badRows)]

  print(paste0(length(badRows)," rows discarded (",round((length(badRows)/nrow(fdata))*100,1),"%)"))
  return(fdata[goodRows,])
}

#' Quality check PORTAL freeway data
#'
#' @param fData Your raw freeway dataframe.
#' @param tolerance A tolerance level of "high", "medium", or "low". "high" is the least conservative tolerance.
#' @param timeCut The time aggregation over which to test quality. Most inuitive strings are accepted, such as "5 min" or "1 hour" or "1 day"
#'
#' @return A list of flags seperated by detector and time period.
#' @export
qualityCheck = function(fData,tolerance ="high",timeCut){
  thresh = vector(length=6)
  names(thresh)=c("count","occupancy","highSpeed","lowSpeed","lowMaxOcc","lowAvgOcc")

  if(tolerance=="high"){
    thresh["count"]=0.002
    thresh["occupancy"]=0.0025
    thresh["highSpeed"]=0.005
    thresh["lowSpeed"]=0.05
    thresh["lowMaxOcc"]=25
    thresh["lowAvgOcc"]=6
  }else if(tolerance=="medium"){
    thresh["count"]=0.003
    thresh["occupancy"]=0.005
    thresh["highSpeed"]=0.01
    thresh["lowSpeed"]=0.1
    thresh["lowMaxOcc"]=20
    thresh["lowAvgOcc"]=5
  }else if (tolerance=="low"){
    thresh["count"]=0.01
    thresh["occupancy"]=0.05
    thresh["highSpeed"]=0.05
    thresh["lowSpeed"]=0.30
    thresh["lowMaxOcc"]=0
    thresh["lowAvgOcc"]=4
  }else{
    stop("Please select 'high', 'medium', or 'low' as a tolerance")
  }

  complete = fData[complete.cases(fData),]

  if(nrow(fData)>0 & nrow(complete)>0){
    dets = unique(fData$detectorid)
    detList = list()
    for (i in 1:length(dets)){
      di = dets[i]
      detector=list()
      detector[["detectorid"]]=di
      subRaw = subset(fData,fData$detectorid == di)
      subRaw$period = as.POSIXct(cut(subRaw$starttime,timeCut))

      timeVec = unique(subRaw$period)
      timeList = list()
      for (j in 1:length(timeVec)){
        timeRaw = subset(subRaw,subRaw$period==timeVec[j])
        time = list()
        time["timePeriod"]= as.character(timeVec[j])

        #Quality Tests
        flags = rep(FALSE,6)
        names(flags)=names(thresh)

        ##Count
        if(as.numeric(quantile(timeRaw$volume,(1-thresh["count"]),na.rm=TRUE))>17){
          flags["count"]=TRUE
        }

        ##Occupancy
        if(as.numeric(quantile(timeRaw$occupancy,(1-thresh["occupancy"]),na.rm=TRUE))>95){
          flags["occupancy"]=TRUE
        }

        ##High Speed
        if(as.numeric(quantile(timeRaw$speed,(1-thresh["highSpeed"]),na.rm=TRUE))>100){
          flags["highSpeed"]=TRUE
        }

        ##Low Speed
        if(as.numeric(quantile(timeRaw$speed,(thresh["lowSpeed"]),na.rm=TRUE))<5){
          flags["lowSpeed"]=TRUE
        }

        ##Low Max Occupancy
        if(max(timeRaw$occupancy,na.rm=TRUE)<thresh["lowMaxOcc"]){
          flags["lowMaxOcc"]=TRUE
        }

        ##Low Avg Occupancy
        timeRaw$hour = hour(timeRaw$starttime)
        peak = subset(timeRaw,(timeRaw$hour >=7 & timeRaw$hour <=9) | (timeRaw$hour >=4 & timeRaw$hour <=6))
        if(nrow(peak[complete.cases(peak),])>0){
          if(mean(peak$occupancy,na.rm=TRUE)<thresh["lowAvgOcc"]){
            flags["lowAvgOcc"]=TRUE
          }
        }

        time[["flags"]]=flags
        timeList[[j]]=time
      }
      detector[["timePeriods"]]=timeList
      detList[[i]]=detector

    }
    return(detList)
  }else{
    stop("No complete observations")
  }
}


