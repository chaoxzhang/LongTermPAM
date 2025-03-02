#' convert MONI-PAM data variables to the correct data format and fill any date or time gaps
#'
#' This function converts MONI-PAM variables to the proper data format and fill any date or time gaps to facilitate data filtering, processing, and visualization.
#'
#' @usage formatPAMdata(PAM.data)
#' @param PAM.data The data can be (1) a combined and organized MONI-PAM dataset generated from the [readPAM] function or after applying [correctF] function, (2) a MONI-PAM dataset after data filtering, generated using filtering functions in this package, such as [filter1.NA], [filter2.night] and so on, (3) an FV/FM dataset generated from the [FindFVFM] function, (4) a diurnal parameter dataset generated from the [diurnalParams] function, or (5) a seasonal parameter dataset generated from the [seasonalParams] function.
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table
#'
#' @return [formatPAMdata] will generate a MONI-PAM data with all the variables in proper data format for later data filtering, processing and visualization
#'
#' @export
formatPAMdata<-function(PAM.data
                          ){
  PAM.data<-data.table(PAM.data)

  # 1. convert columns to correct data format
  if ('datetime'%in%names(PAM.data)){
    ##<<- convert 'date'|'dateBack12h' from factor to Date format
    PAM.data$date<-as.Date(PAM.data$date)
    PAM.data$dateBack12h<-ymd(PAM.data$dateBack12h)

    ##<<- convert 'datetime'|'datetimeBack12h'|'sunrise'|'sunsetStart'
    ##<<- from factor to timeseries using ymd_hms() from library(lubridate)
    for (i in c("datetime","datetimeBack12h")){

      PAM.data[[i]]<-ymd_hms(PAM.data[[i]],truncated = 3)
    }

    for (i in c("sunrise","sunriseEnd","solarNoon",
                "sunsetStart","sunset","dusk","dawn")){

      PAM.data[[i]]<-ymd_hms(PAM.data[[i]],truncated = 3)
    }


  } else {
    PAM.data$date<-as.Date(PAM.data$date)
  }

  PAM.data$tree_num<-as.factor(PAM.data$tree_num)
  PAM.data$head<-as.factor(PAM.data$head)

  if ('head_tree'%in%names(PAM.data)){
    PAM.data$head_tree<-as.factor(PAM.data$head_tree)
  } else {
    PAM.data$head_tree<-
      as.factor(paste0(PAM.data$head,'_',PAM.data$tree_num))
  }

  # 2. fill the time gap for each head and tree_num for plotting

  if ('datetime'%in%names(PAM.data)){
    range.date.data<-PAM.data %>% dplyr::select(datetime,head,tree_num) %>% as.data.table()
    range.date<-
      range.date.data %>%
      group_by(head,tree_num) %>%
      dplyr::summarise(start.date=min(datetime,na.rm = T),
                       end.date=max(datetime,na.rm = T),
                       .groups = 'drop')
    filldate<-
      ldply(1:nrow(range.date),function(i){
        range.i<-range.date[i,]
        Time=seq(ymd_hms(paste(lubridate::date(range.i$start.date),
                               lubridate::hour(range.i$start.date),':00:00')),
                 ymd_hms(paste(lubridate::date(range.i$end.date),
                               lubridate::hour(range.i$end.date),':00:00')),
                 by='hour')
        fill.data<-PAM.data %>%subset(head==range.i$head&tree_num==range.i$tree_num)

        #add a column of Year month day and hour,
        #later this will be the filled column
        fill.data$ymdh<-ymd_hms(paste0(lubridate::date(fill.data$datetime),
                                       lubridate::hour(fill.data$datetime),':00:00'))
        #use complete function from tidyr package
        fill.dategap<-complete(fill.data,ymdh=Time,nesting(head,tree_num))
        #fill empty date column
        fill.dategap$date[is.na(fill.dategap$date)]<-
          lubridate::date(fill.dategap$ymdh[is.na(fill.dategap$date)])
        #fill empty datetime column
        fill.dategap$datetime[is.na(fill.dategap$datetime)]<-
          fill.dategap$ymdh[is.na(fill.dategap$datetime)]
        #remove ymdh column
        fill.dategap<-fill.dategap[,-1]

        return(fill.dategap)
        })
    # fill the gaps for plot.group
    filldate$groups<-NA
    filldate$groups[day(filldate$datetime)<=10&
                                day(filldate$datetime)>0]<-'a'
    filldate$groups[day(filldate$datetime)<=20&
                                day(filldate$datetime)>10]<-'b'
    filldate$groups[day(filldate$datetime)<=31&
                                day(filldate$datetime)>20]<-'c'
    filldate$plot.group<-
      paste(year(filldate$datetime),
            month(filldate$datetime),
            filldate$groups,sep = '-')
    filldate$plot.group<-as.factor(filldate$plot.group)
    filldate$head_tree<-
      as.factor(paste0(filldate$head,'_',filldate$tree_num))
    filldate<-filldate %>% select(-groups)
    # fill the gaps for datetimeBack12h and dateBack12h
    filldate$datetimeBack12h<- filldate$datetime-12*3600
    filldate$dateBack12h<-ymd(lubridate::date(filldate$datetimeBack12h))
    filldate<- filldate[order(filldate$datetime),]

    } else { # if 'datetime' is not in PAM.data, it means the data if FVFM data, so we only file the date gap

    range.date.data<-PAM.data %>% dplyr::select(date,head,tree_num) %>% as.data.table()
    range.date<-
      range.date.data %>%
      group_by(head,tree_num) %>%
      dplyr::summarise(start.date=min(date,na.rm = T),
                       end.date=max(date,na.rm = T),
                       .groups = 'drop')
    filldate<-
      ldply(1:nrow(range.date),function(i){
        range.i<-range.date[i,]
        Time=seq(lubridate::date(range.i$start.date),
                 lubridate::date(range.i$end.date),by='day')
        fill.data<-PAM.data %>%subset(head==range.i$head&tree_num==range.i$tree_num)

        #add a column of Year month day,
        #later this will be the filled column
        fill.data$ymd<-lubridate::date(fill.data$date)
        #use complete function from tidyr package
        fill.dategap<-complete(fill.data,ymd=Time,nesting(head,tree_num))
        #fill empty date column
        fill.dategap$date[is.na(fill.dategap$date)]<-
          lubridate::date(fill.dategap$ymd[is.na(fill.dategap$date)])
        #fill empty datetime column
        fill.dategap$date[is.na(fill.dategap$date)]<-
          fill.dategap$ymd[is.na(fill.dategap$date)]
        #remove ymdh column
        fill.dategap<-fill.dategap[,-1]

        return(fill.dategap)
        })
    filldate$head_tree<-
          as.factor(paste0(filldate$head,'_',filldate$tree_num))
    }

  return(filldate)
}
