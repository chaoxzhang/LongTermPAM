
#' filter logic function for filter2.night function
#' @export
filter.night.fc<-function(data,period,f2.Fm,f2.FmYII){

  data<-data[data$flag2.night==1,]

  PAM.night<-na.omit(data[data$datetime%in%period,
                           c('dateBack12h','datetime','F_','Fm_',
                             'YII','flag2.night')])

  PAM.night<-ldply(unique(PAM.night$dateBack12h),function(i){
    #i=1
    PAM.night.oneday<-PAM.night[PAM.night$dateBack12h==i,]
    PAM.night.oneday<-
      PAM.night.oneday[order(PAM.night.oneday$datetime),]

    PAM.night.oneday$vary.YII<-
      c(NA,diff(PAM.night.oneday$YII))/PAM.night.oneday$YII
    PAM.night.oneday$vary.Fm<-
      c(NA,diff(PAM.night.oneday$Fm_))/PAM.night.oneday$Fm_
    PAM.night.oneday$vary.F<-
      c(NA,diff(PAM.night.oneday$F_))/PAM.night.oneday$F_
    if (f2.Fm>=0&
        f2.FmYII>=0){


      ##> impossible cases
      ##> 1. YII increases, but Fm' decreases a lot and F' decreases
      ##> 2. Fm' decreases a lot, F' decreases, and decreases in Fm'
      ##     is f2.FmYII times of decreases in YII
      PAM.night.oneday$flag2.night[
        (PAM.night.oneday$vary.YII>=0&
           PAM.night.oneday$vary.Fm<=(-1*f2.Fm)&
           PAM.night.oneday$vary.F<=0)|
          (PAM.night.oneday$vary.Fm<=(-1*f2.Fm)&
             PAM.night.oneday$vary.F<=0&
             PAM.night.oneday$vary.Fm/PAM.night.oneday$vary.YII>=
             f2.FmYII)]<-0
    }
    return(PAM.night.oneday)
  })
  data$flag2.night[
    data$datetime%in%
      (PAM.night$datetime[PAM.night$flag2.night==0])]<-0
  return(data)
}

#' Filter PAM-PAM data step 2,remove spurious F', Fm' and Y(II) value from the night
#'
#'
#' @usage filter2.night(PAM.data,f2.Fm=0.03,f2.FmYII=3,save.path,save.file)
#' @param PAM.data a data.table or data.frame PAM-PAM data generated from [filter1.lowF] function.
#' @param f2.Fm the threshold of percentage change of Fm' between consecutive points. Default value is 0.03, we recommend this argument can be adjusted from 0.01 to 0.1 by an interval of 0.01.
#' @param f2.FmYII the threshold of ratio between percentage change in Fm' between consecutive points and in Y(II) between consecutive points. Default value is 3, we recommend this argument can be set as default value or adjusted between 2 and 5 by an interval of 1
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder

#' @importFrom plyr ldply
#'
#' @return [filter2.night] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'PAM_Year1_Year2_filter2night.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter1.lowF] function named as 'flag2.night'. This column only contain two values: 0 and 1, where 0 means F', Fm' and YII in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter2.night<-function(PAM.data,
                        f2.Fm=0.03,
                        f2.FmYII=3,
                        save.path,
                        save.file)
{
  print('This function will run mins...')
  start.time<-Sys.time()
  PAM.data<-formatPAMdata(PAM.data = PAM.data)

  night.filter<-
    ldply(levels(PAM.data$head_tree),function(i){
      #i=levels(PAM.data$head_tree)[2]
      PAM.onetree <- PAM.data[PAM.data$head_tree==i,]

      if (nrow(PAM.onetree)>0) {#if data exsit

        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        PAM.onetree<-PAM.onetree[order(PAM.onetree$datetime),]
        PAM.onetree$flag2.night<-1

        # one hour buffer added to sunset time, but not dawn time because
        # it will filter the good data between dawn and sunrise
        flag.period<-
          PAM.onetree$datetime[
            -which(PAM.onetree$datetime<=PAM.onetree$sunsetStart-3600&
                     PAM.onetree$datetime>=PAM.onetree$dawn)]

        filter.onetree<-
          filter.night.fc(data=PAM.onetree, period=flag.period,
                          f2.Fm=f2.Fm,f2.FmYII=f2.FmYII)
        trials <- 0
        print(paste0(i, ' is filtering...'))
        while(nrow(filter.onetree[filter.onetree$flag2.night==0,])>0){
          filter.onetree<-
            filter.night.fc(data=filter.onetree,period=flag.period,
                            f2.Fm=f2.Fm,f2.FmYII=f2.FmYII)
          trials <- trials +1
        }
        PAM.onetree$flag2.night[
          !PAM.onetree$datetime%in%filter.onetree$datetime]<-0

        return(PAM.onetree)
      }
    }
    )


  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}

  night.filter[night.filter$flag2.night==0,c('F_','Fm_','YII')]<-NA

  night.filter$flag.all<-
    night.filter$flag1.lowF*night.filter$flag2.night

  night.filter<-
    droplevels(night.filter[,c("date", "plot.group","dateBack12h","datetime",
                               "datetimeBack12h","head", "tree_num",
                               "sunrise","sunriseEnd",
                               "solarNoon","sunsetStart",
                               "sunset","dusk",
                               "dawn","F_","Fm_",
                               "YII", "par_PAM","temp_PAM","ETR",
                               'head_tree',"flag1.lowF",
                               'flag2.night','flag.all')])
  if (save.file==T){
    write.table(night.filter,row.names = F,sep = ';', file =
                  paste0(save.path,'/PAM_',year(range(night.filter$date)[1]),
                         '_',year(range(night.filter$date)[2]),
                         '_filter2night.dat'))

  }
  print(Sys.time()-start.time)
  return(night.filter)
}
