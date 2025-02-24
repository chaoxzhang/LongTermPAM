#' filter logical function for filter5.expand
#' @export
filter.expand.fc<-function(data,f5.fm,f5.fmYII){
  data=data[
    data$flag5.expand==1,c('date','datetime','F_','Fm_','YII','flag5.expand')]
  data<-data[order(data$datetime),]
  data$ID<-c(1:nrow(data))
  data.filter<-na.omit(data)
  data.filter$dif.ID<- c(NA,diff(data.filter$ID))
  #calculate percentage changes in YII, Fm', and F'
  data.filter$vary.YII<-c(NA,diff(data.filter$YII))/data.filter$YII
  data.filter$vary.Fm<-c(NA,diff(data.filter$Fm_))/data.filter$Fm_
  data.filter$vary.F<-c(NA,diff(data.filter$F_))/data.filter$F_
  ##>> select data only when at least 2 points have been filtered by
  ##   filter functions 1 to 4
  data.filter<-data.filter[data.filter$dif.ID>=3,]

  if (f5.fm>=0&
      f5.fmYII>0){
    data.filter$flag5.expand[
      (data.filter$vary.YII>=0&data.filter$vary.Fm<=(-1*f5.fm))|
        (data.filter$vary.Fm<=(-1*f5.fm)&
           data.filter$vary.Fm/data.filter$vary.YII>=f5.fmYII)]<-0
  }


  data$flag5.expand[
    data$datetime%in%(data.filter$datetime[data.filter$flag5.expand==0])]<-0
  return(data)
}

#' Filter MONI-PAM data step 5,remove additionally abnormal low Fm’ data that is or are adjacent to previously filtered data
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @usage filter5.expand(PAM.data,save.path,save.file,f5.fm=0.2,f5.fmYII=3)
#' @param PAM.data a data.table or data.frame MONI-PAM data generated from [filter4.FvFm] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#' @param f5.fm the threshold of percentage change of Fm' between point2 and point1. Default value is 0.2, we recommend this argument can be adjusted from 0.05 to 0.3 by an interval of 0.05.
#' @param f5.fmYII the threshold of ratio between percentage change of Fm' between point2 and point1 and of YII between point2 and point1. Default value is 3, we recommend this argument can be adjust between 2 and 5 by an interval of 1.
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom plyr ldply
#'
#' @return [filter5.expand] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'PAM_Year1_Year2_filter5expand.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter4.FvFm] function named as 'flag5.expand'. This column only contain two values: 0 and 1, where 0 means F', Fm' and YII in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export

filter5.expand<-function(PAM.data,
                        save.path,
                        save.file,
                        f5.fm=0.2,
                        f5.fmYII=3){
  start.time<-Sys.time()
  print('This function will run mins')
  PAM.data<-formatPAMdata(PAM.data = PAM.data)
  expand.filter<-
    ldply(levels(PAM.data$head_tree),function(i){
      ##<<- the data should be filtered following the
      ##<<- correct date and time order
      ##<<- so order() function is used here to make sure correct order
      PAM.onetree<- PAM.data[PAM.data$head_tree==i,]


      if (nrow(PAM.onetree)>0) {#if data exsit

        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        PAM.onetree<-PAM.onetree[order(PAM.onetree$datetime),]
        PAM.onetree$flag5.expand<-1

        filter.onetree<-
          filter.expand.fc(data=PAM.onetree,f5.fm=f5.fm,f5.fmYII=f5.fmYII)
        trials <- 0
        print(paste0(i, ' is filtering...'))
        while(nrow(filter.onetree[filter.onetree$flag5.expand==0,])>0){
          filter.onetree<-
            filter.expand.fc(data=filter.onetree,f5.fm=f5.fm,f5.fmYII=f5.fmYII)
          trials <- trials +1
        }

        PAM.onetree$flag5.expand[
          !PAM.onetree$datetime%in%filter.onetree$datetime]<-0

      }

      return(PAM.onetree)
    }
    )
  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  expand.filter[expand.filter$flag5.expand==0,c('F_','Fm_','YII')]<-NA

  expand.filter$flag.all<-
    expand.filter$flag1.lowF.YII*expand.filter$flag2.night*
    expand.filter$flag3.day*expand.filter$flag4.FvFm*expand.filter$flag5.expand


  expand.filter<-
    droplevels(expand.filter[,c("date", "plot.group","dateBack12h","datetime",
                               "datetimeBack12h","head", "tree_num",
                               "sunrise","sunriseEnd",
                               "solarNoon","sunsetStart",
                               "sunset","dusk",
                               "dawn","F_","Fm_",
                               "YII", "par_PAM","temp_PAM","ETR",
                               'head_tree',"flag1.lowF.YII",
                               'flag2.night','flag3.day',
                               'flag4.FvFm',
                               'flag5.expand','flag.all')])
  if (save.file==T){
    write.table(expand.filter,file =
                  paste0(save.path,'/PAM_',year(range(expand.filter$date)[1]),
                         '_', year(range(expand.filter$date)[2]),
                         '_filter5expand.dat'),
                row.names = F,sep = ';')

  }
  print(Sys.time()-start.time)
  return(expand.filter)
}
