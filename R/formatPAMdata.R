#' convert MONI-PAM data variables to correct data format
#'
#' This function will convert the MONI-PAM variables to proper data format for easily used for data filtering and processing
#'
#' @usage formatPAMdata(PAM.data)
#' @param PAM.data a combined organized MONI-PAM data which is generated from [readPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.lowF], [filter2.night] and so on.
#'
#' @import lubridate
#' @import data.table
#'
#' @return [formatPAMdata] will generate a MONI-PAM data with all the variables in proper data format for later data filtering and processing
#'
#' @export
formatPAMdata<-function(PAM.data
                          ){
  PAM.data<-data.table(PAM.data)
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
      as.factor(paste0('H',PAM.data$head,'_',PAM.data$tree_num))
  }
  return(PAM.data)
}
