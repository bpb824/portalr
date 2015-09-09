#' Query and return freeway loop and radar data from PORTAL
#'
#' @param con Database connection object.
#' @param dets Vector of detector IDs.
#' @param startTime Start time of requested data in either a YYYY-MM-DD or YYYY-MM-DD HH:MM:SS format.
#' @param endTime End time of requested data in either a YYYY-MM-DD or YYYY-MM-DD HH:MM:SS format.
#'
#' @return Result of freeway data query.
#' @export
#'
freewayData = function(con,dets,startTime,endTime){

  #Start time querying parameter
  if(identical(as.character(as.Date(startTime)),startTime)){
    qStart = paste0("'",startTime,"T00:00:00","'")
  }else{
    split = unlist(strsplit(startTime," "))
    qStart = paste0("'",split[1],"T",split[2],"'")
  }

  #End time querying parameter
  if(identical(as.character(as.Date(endTime)),endTime)){
    qEnd = paste0("'",endTime,"T23:59:59","'")
  }else{
    split = unlist(strsplit(endTime," "))
    qEnd = paste0("'",split[1],"T",split[2],"'")
  }

  query = paste0("SELECT * FROM freeway.data WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
  if(length(dets)>1){
    for (i in 2:length(dets)){
      query = paste0(query," OR detectorid = ",dets[i])
    }
  }
  query = paste0(query,")")

  result = DBI::dbGetQuery(con,query)

  #No data in freeway table, try public table
  if(nrow(result)==0){
    startDate=as.Date(startTime)
    endDate = as.Date(endTime)
    if(startDate>=as.Date("2015-07-01")){
      stop("public table does not have data after June 2015 and freeway table does not have your data, try another query")
    }
    if(endDate>=as.Date("2015-07-01")){
      endDate = "2015-06-30"
      print("public table does not have data after June 2015, truncating data")
    }
    startMonth= gsub("-","_",substr(startDate,1,7))
    endMonth= gsub("-","_",substr(endDate,1,7))
    if(startMonth==endMonth){
      query = paste0("SELECT * FROM public.loopdata_",startMonth," WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
      if(length(dets)>1){
        for (k in 2:length(dets)){
          query = paste0(query," OR detectorid = ",dets[k])
        }
      }
      query = paste0(query,")")
      result = dbGetQuery(con,query)
    }else{
      monthVec = timeDate::timeSequence(startDate,endDate,by="month")
      monthVec = gsub("-","_",substr(monthVec,1,7))
      dataList = list()
      query = paste0("SELECT * FROM public.loopdata_",monthVec[1]," WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
      if(length(dets)>1){
        for (k in 2:length(dets)){
          query = paste0(query," OR detectorid = ",dets[k])
        }
      }
      query = paste0(query,")")
      subResult = dbGetQuery(con,query)
      dataList[[1]]=subResult
      for (i in 2:length(monthVec)){
        query = paste0("SELECT * FROM public.loopdata_",monthVec[i]," WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
        if(length(dets)>1){
          for (k in 2:length(dets)){
            query = paste0(query," OR detectorid = ",dets[k])
          }
        }
        query = paste0(query,")")
        subResult = dbGetQuery(con,query)
        dataList[[i]]=subResult
      }
      rowCount = 0
      for(i in 1:length(dataList)){
        rowCount = rowCount+nrow(dataList[[i]])
      }
      result = data.frame(matrix(nrow=rowCount,ncol=ncol(dataList[[1]])))
      colnames(result)=colnames(dataList[[1]])
      ri = 1
      for (i in 1:length(dataList)){
        result[ri:(ri+nrow(dataList[[i]])-1),]=dataList[[i]]
        ri = ri+nrow(dataList[[i]])
      }
    }
  } else{
    startDate=as.Date(startTime)
    endDate = as.Date(endTime)
    #Test if early data not available in freeway table, grab from public table
    if(as.Date(as.POSIXct(min(result$starttime),origin="1970-01-01",tz="America/Los_Angeles"))>as.Date(startDate)){
      subEnd = as.Date(as.POSIXct(min(result$starttime),origin="1970-01-01",tz="America/Los_Angeles"))
      startMonth= gsub("-","_",substr(startDate,1,7))
      endMonth= gsub("-","_",substr(subEnd,1,7))
      qStart = paste0("'",startDate,"T00:00:00","'")
      qEnd = paste0("'",subEnd,"T23:59:59","'")
      if(startMonth==endMonth){
        query = paste0("SELECT * FROM public.loopdata_",startMonth," WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
        if(length(dets)>1){
          for (k in 2:length(dets)){
            query = paste0(query," OR detectorid = ",dets[k])
          }
        }
        query = paste0(query,")")
        result = dbGetQuery(con,query)
      }else{
        monthVec = timeDate::timeSequence(startDate,endDate,by="month")
        monthVec = gsub("-","_",substr(monthVec,1,7))
        dataList = list()
        query = paste0("SELECT * FROM public.loopdata_",monthVec[1]," WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
        if(length(dets)>1){
          for (k in 2:length(dets)){
            query = paste0(query," OR detectorid = ",dets[k])
          }
        }
        query = paste0(query,")")
        subResult = dbGetQuery(con,query)
        dataList[[1]]=subResult
        for (i in 2:length(monthVec)){
          query = paste0("SELECT * FROM public.loopdata_",monthVec[i]," WHERE (starttime >=",qStart," AND starttime <=",qEnd,") AND (detectorid = ",dets[1])
          if(length(dets)>1){
            for (k in 2:length(dets)){
              query = paste0(query," OR detectorid = ",dets[k])
            }
          }
          query = paste0(query,")")
          subResult = dbGetQuery(con,query)
          dataList[[i]]=subResult
        }
        rowCount = 0
        for(i in 1:length(dataList)){
          rowCount = rowCount+nrow(dataList[[i]])
        }
        pubResult = data.frame(matrix(nrow=rowCount,ncol=ncol(dataList[[1]])))
        colnames(pubResult)=colnames(dataList[[1]])
        ri = 1
        for (i in 1:length(dataList)){
          pubResult[ri:(ri+nrow(dataList[[i]])-1),]=dataList[[i]]
          ri = ri+nrow(dataList[[i]])
        }
      }
      result = rbind(result,pubResult)
    }

  }
  return(result)
}

ttData = function(){

}

