
#' filter logical function 1 for filter3.day function
#' @export
filter.o.fc<-function(data,f3.Fm,f3.FmYII){

  data<-
    data %>%
    subset(flag3.day==1,select=c('date','datetime','dateBack12h','datetimeBack12h',
                                'solarNoon','dawn','sunrise',
                                'sunsetStart', 'dusk',
                                'temp_PAM','par_PAM', 'F_','Fm_',
                                'YII','flag3.day'))

  PAM.day<-ldply(unique(data$date),function(i){

    oneday<-
      data %>%
      subset(date==i&datetime>=dawn&datetime<=dusk) %>%
      na.omit()
    if (isTRUE(nrow(oneday)>0)){ # run following code only if data exist

      oneday$ID<-1:nrow(oneday)
      oneday<-oneday[order(oneday$datetime),]

      #calculate percentage changes in YII, Fm', and F'
      oneday$vary.YII<-c(NA,diff(oneday$YII))/oneday$YII
      oneday$vary.Fm<- c(NA,diff(oneday$Fm_))/oneday$Fm_
      oneday$vary.F<-c(NA,diff(oneday$F_))/oneday$F_
      #select data when Fm has consecutively decrease and/or increase.
      fm.dec<-oneday[with(rle(oneday$vary.Fm<0&oneday$vary.YII>0),
                          rep(lengths, lengths))>=2&
                       with(rle(oneday$vary.Fm<0&oneday$vary.YII>0),
                            rep(values, lengths))==TRUE,]
      fm.inc<-oneday[with(rle(oneday$vary.Fm>0),rep(lengths, lengths))>=2&
                       with(rle(oneday$vary.Fm>0),
                            rep(values, lengths))==TRUE,]
      if (isTRUE(nrow(fm.inc)>0)){
        inc.groupID<-
          do.call(rbind,by(fm.inc,cumsum(c(0,diff(fm.inc$ID)!=1)),
                           function(g) data.frame(IDmin=min(g$ID),IDmax=max(g$ID),
                                                  IDrange=diff(range(g$ID)))))
        fm.inc<-
          ldply(1:nrow(inc.groupID),function(i){
            if(isTRUE(nrow(inc.groupID)>0)){
              fm.inc<-fm.inc[fm.inc$ID>=inc.groupID[i,]$IDmin&
                               fm.inc$ID<=inc.groupID[i,]$IDmax,]
            } else {fm.inc=fm.inc}
            sum.fm<-sum(fm.inc$vary.Fm,na.rm=T)
            sum.YII.dec<-
              sum(abs(fm.inc$vary.YII[fm.inc$vary.YII<0]),na.rm=T)
            # total increase in Fm is equal or more than 20% and
            # total decrease in YII is equal or more than 10%
            if (isTRUE(sum.fm>=0.2&sum.YII.dec>=0.1)){return(fm.inc)}

          })}

      if(isTRUE(nrow(fm.dec)>0)){
        dec.groupID<-
          do.call(rbind,by(fm.dec,cumsum(c(0,diff(fm.dec$ID)!=1)),
                           function(g) data.frame(IDmin=min(g$ID),IDmax=max(g$ID),
                                                  IDrange=diff(range(g$ID)))))
        fm.dec<-
          ldply(1:nrow(dec.groupID),function(i){
            if(isTRUE(nrow(dec.groupID)>0)){
              fm.dec<-fm.dec[fm.dec$ID>=dec.groupID[i,]$IDmin&
                               fm.dec$ID<=dec.groupID[i,]$IDmax,]
            } else {fm.dec=fm.dec}
            sum.fm<-abs(sum(fm.dec$vary.Fm,na.rm = T))
            sum.YII.inc<-sum(abs(fm.dec$vary.YII[fm.dec$vary.YII>0]))

            # total decrease in Fm is equal or more than 10% and
            # total increase in YII is equal or more than 10%
            if (isTRUE(sum.fm>=0.1&sum.YII.inc>=0.1&
                TRUE%in%(max(fm.dec$datetime,na.rm=T)-fm.inc$datetime<=0)))
            {return(fm.dec)}

          })}


      #apply the filter function when both consecutive decrease and increase exist

      if (isTRUE(nrow(fm.inc)>0&nrow(fm.dec)>0)){
        if(isTRUE(TRUE%in%(max(fm.dec$datetime,na.rm=T)-fm.inc$datetime<0))){
          data.select<-
            oneday[oneday$datetime>=min(fm.dec$datetime)&
                     oneday$datetime<=max(fm.inc$datetime),]

          data.select<-data.select[order(data.select$datetime),]
          data.select<-
            oneday[
              #also expand the focused data go back one more point
              c(which(oneday$datetime%in%data.select$datetime)[1]-1,
                which(oneday$datetime%in%data.select$datetime)),]
          # ***********************************************************
          ##>> impossible case
          ##>> 1. YII increases then decreases, Fm' and F' decreases
          ##>>    than increases a lot
          if (isTRUE(f3.Fm>=0&f3.FmYII>=1)){

            data.select$flag3.day[
              (data.select$vary.YII>=0&
                 data.select$vary.Fm<=(-1*f3.Fm))|
                (data.select$vary.YII<=0&
                   data.select$vary.Fm>=f3.Fm)|
                (data.select$vary.Fm<=(-1*f3.Fm)&
                   data.select$vary.Fm/data.select$vary.YII>=f3.FmYII)]<-0
          }
          data.select2<-data.select
          # repeat for all the data until all the abnormal points were found
          trials <- 0
          while(nrow(data.select2[data.select2$flag3.day==0,])>0){
            data.select2<-data.select2[data.select2$flag3.day==1,]
            data.select2<-data.select2[order(data.select2$datetime),]
            #calculate percentage changes in YII, Fm', and F'
            data.select2$vary.YII<-
              c(NA,diff(data.select2$YII))/data.select2$YII
            data.select2$vary.Fm<-
              c(NA,diff(data.select2$Fm_))/data.select2$Fm_
            data.select2$vary.F<-
              c(NA,diff(data.select2$F_))/data.select2$F_
            data.select2$flag3.day[
              (data.select2$vary.YII>=0&
                 data.select2$vary.Fm<=(-1*f3.Fm))|
                (data.select2$vary.YII<=0&
                   data.select2$vary.Fm>=f3.Fm)|
                (data.select2$vary.Fm<=(-1*f3.Fm)&
                   data.select2$vary.Fm/data.select2$vary.YII>=f3.FmYII)
            ]<-0
            trials <- trials +1
          }
          oneday$flag3.day[oneday$datetime%in%
                             data.select$datetime[!data.select$datetime%in%
                                                    data.select2$datetime]]<-0

        }}
      return(oneday)
    }

  })
  data$flag3.day[
    data$datetime%in%(PAM.day$datetime[PAM.day$flag3.day==0])]<-0
  return(data)
}
#' filter logical function 2 for filter3.day function
#' @export
filter.pm.fc<-function(data,f3.YII,f3.Fm){

  data<-
    data %>%
    subset(flag3.day==1,select=c('date','datetime','dateBack12h',
                                 'datetimeBack12h','solarNoon','dawn','sunrise',
                                 'sunsetStart', 'dusk','temp_PAM','par_PAM',
                                 'F_','Fm_','YII','flag3.day'))

  PAM.pm<-ldply(unique(data$date),function(i){
    day.pm<-
      data %>%
      subset(date==i&datetime>=solarNoon&datetime<=dusk+3600) %>%
      na.omit()
    if (isTRUE(nrow(day.pm)>0)){ # run following code only if data exist

      day.pm<-day.pm[order(day.pm$datetime),]

      day.pm$vary.YII<-c(NA,diff(day.pm$YII))/day.pm$YII
      day.pm$vary.Fm<-c(NA,diff(day.pm$Fm_))/day.pm$Fm_
      day.pm$vary.F<- c(NA,diff(day.pm$F_))/day.pm$F_
      if (isTRUE(f3.YII>=0&f3.Fm>=0)){

        ##> impossible cases
        ##> 1. YII increases or has slightly changes, but Fm' decreases a lot
        day.pm$flag3.day[(day.pm$vary.YII>=(-1*f3.YII)&
                            day.pm$vary.Fm<=(-1*f3.Fm))]<-0

      }
    }
    return(day.pm)
  })
  data$flag3.day[
    data$datetime%in%(PAM.pm$datetime[PAM.pm$flag3.day==0])]<-0

  return(data)
}
#' filter logical function 3 for filter3.day function
#' @export
filter.v.fc<-function(data){

  #selected data that not filtered from previous functions
  data<-
    data %>%
    subset(flag3.day==1,select=c('datetime','dateBack12h','datetimeBack12h',
                                 'solarNoon','dawn','sunrise',
                                 'sunsetStart',
                                 'temp_PAM','par_PAM', 'F_','Fm_',
                                 'YII','flag3.day'))

  PAM.day<-ldply(unique(data$dateBack12h),function(d){#data from DAY1 12:00 to DAY2 12:00

    one.day<-na.omit(data[data$dateBack12h==d,])
    #only focus data from DAY1sunset to DAY2-solarnoon
    #during the condensation easily occurred period
    one.day<-one.day[one.day$datetimeBack12h>=one.day$sunsetStart[1]-12*3600,]

    oneday<-one.day

    trial<-0
    while(nrow(oneday)>0){
      if (isTRUE(exists('fm.v'))){rm(fm.v)}
      oneday<-oneday[oneday$flag3.day==1,]
      oneday<-oneday[order(oneday$datetime),]
      oneday$ID<-1:nrow(oneday)
      #calculate percentage changes in YII, Fm', and F'
      oneday$vary.YII<-c(NA,diff(oneday$YII))/oneday$YII
      oneday$vary.Fm<- c(NA,diff(oneday$Fm_))/oneday$Fm_
      oneday$vary.F<-c(NA,diff(oneday$F_))/oneday$F_
      oneday$vary.temp<-c(NA,diff(oneday$temp_PAM))/oneday$temp_PAM
      oneday$vary.par<-0
      oneday$vary.par[oneday$par_PAM>0]<-
        c(NA,diff(oneday$par_PAM[oneday$par_PAM>0]))/
        oneday$par_PAM[oneday$par_PAM>0]

      #select data when Fm has consecutively decrease and/or increase.
      fm.dec<-oneday[which(with(rle(oneday$vary.Fm<0),rep(lengths, lengths))>=1&
                             with(rle(oneday$vary.Fm<0),
                                  rep(values, lengths))==TRUE),]
      fm.inc<-oneday[which(with(rle(oneday$vary.Fm>0),rep(lengths, lengths))>=1&
                             with(rle(oneday$vary.Fm>0),
                                  rep(values, lengths))==TRUE),]
      if (isTRUE(nrow(fm.inc)>0&nrow(fm.dec)>0)){
        inc.groupID<-
          do.call(rbind,by(fm.inc,cumsum(c(0,diff(fm.inc$ID)!=1)),
                           function(g) data.frame(IDmin=min(g$ID),IDmax=max(g$ID),
                                                  IDrange=diff(range(g$ID)))))
        dec.groupID<-
          do.call(rbind,by(fm.dec,cumsum(c(0,diff(fm.dec$ID)!=1)),
                           function(g) data.frame(IDmin=min(g$ID),IDmax=max(g$ID),
                                                  IDrange=diff(range(g$ID)))))

        fm.v<-
          ldply(1:nrow(dec.groupID),function(i){

            if(isTRUE(nrow(dec.groupID)>0)){
              #find the index (or row number) of decreased Fm
              fm.dec.i<-fm.dec[fm.dec$ID>=dec.groupID[i,]$IDmin&
                                 fm.dec$ID<=dec.groupID[i,]$IDmax,]
            } else {fm.dec.i=fm.dec}
            dec.start<-fm.dec.i$ID[1]
            mean.dec<-mean(abs(fm.dec.i$vary.Fm[fm.dec.i$ID>=dec.start]))

            if (isTRUE(!is.na(dec.start)&
                mean.dec>=0.03&
                #if decrease followed by an increase
                TRUE%in%(dec.groupID[i,2]<inc.groupID$IDmin)
            )){
              inc.end<-min(inc.groupID$IDmax[dec.groupID[i,2]<inc.groupID$IDmin])
              #select the V shape data (fm consecutively decrease then increase)
              fm.v<-oneday[oneday$ID>=dec.start-1&oneday$ID<=inc.end,]
              maxF.ID<-fm.v$ID[which.max(fm.v$F_)]
              maxF.time<-fm.v$datetime[which.max(fm.v$F_)]
              maxF.dawn<-unique(fm.v$dawn[fm.v$datetime==maxF.time])
              maxF.sunrise<-unique(fm.v$sunrise[fm.v$datetime==maxF.time])

              if (isTRUE(maxF.ID<max(fm.v$ID))){
                maxF.varyPAR<-
                  fm.v$par_PAM[fm.v$ID==maxF.ID+1]-fm.v$par_PAM[fm.v$ID==maxF.ID]
                maxF.varyF<-
                  fm.v$F_[fm.v$ID==maxF.ID+1]-fm.v$F_[fm.v$ID==maxF.ID]
              } else {
                maxF.varyPAR<-0
                maxF.varyF<-0
              }

              minF.ID<-fm.v$ID[fm.v$F_==min(fm.v$F_[fm.v$vary.Fm<0],na.rm = T)]
              minFm.ID<-fm.v$ID[fm.v$Fm_==min(fm.v$Fm_[fm.v$vary.Fm<0],na.rm = T)]
              #PAR was increasing in V shape period
              meanPAR<-mean(fm.v$par_PAM)
              sum.decFm<-sum(abs(fm.v$vary.Fm[fm.v$vary.Fm<=0]),na.rm = T)
              if(isTRUE(TRUE%in%(fm.v$vary.Fm>0.03))) {
                mean.incFm<-mean(fm.v$vary.Fm[fm.v$vary.Fm>0.03],na.rm=T)
              } else {
                mean.incFm=mean(fm.v$vary.Fm[fm.v$vary.Fm>0],na.rm=T)
              }
              if (isTRUE(!(maxF.time>=maxF.dawn&
                    maxF.ID%in%fm.v$ID[fm.v$vary.Fm<0]&
                    maxF.ID<min(minF.ID)&
                    maxF.varyF<0)&# this part means if all conditions occurred,
                  # inside v shape, max(F) appears in the beginning (followed by
                  # decrease and increase) but F will not go back to that max(F)
                  #in the end of V shape period. Such changes were quite often
                  #happening in spring when F in the night was much higher than
                  #in the morning and they are normal data instead of abnormal observations.
                  sum.decFm>=0.2&
                  (meanPAR<=20|# there is tiny light or no light
                   (meanPAR>20&#there is stronger light
                    !(fm.v$ID[which.max(fm.v$par_PAM)]>fm.v$ID[1]&
                      fm.v$ID[which.max(fm.v$par_PAM)]<last(fm.v$ID)))#if max
                   #PAR not in the middle of the data,i.e., shape of PAR changes
                   #was not like "increased then decreased"
                  )&
                  mean.incFm>0.05

              )){# make sure last few points are not [good point (vary.Fm<=0.03,
                # namely did not change so much)]
                #m=(1:nrow(fm.v))[1]]
                fm.v<-fm.v[-1,]
                rmStart.index<-NA
                for(m in (1:nrow(fm.v))) {
                  if(isTRUE(abs(fm.v$vary.Fm[m])>0.03)){#if changes in start is small,
                    #this data will not be removed,i.e., this data will be removed
                    # from fm.v
                    break
                  } else {rmStart.index[m]<-m}
                }

                if (isTRUE(FALSE%in%is.na(rmStart.index))){
                  fm.v<-fm.v[-rmStart.index,]
                }

                rmEnd.index<-NA
                for(n in (nrow(fm.v):1)) {
                  #n=(nrow(fm.v):1)[2]
                  if(isTRUE(abs(fm.v$vary.Fm[n])>0.03)){
                    break
                  } else {rmEnd.index[n]<-n}#if changes in the end of v shape is
                  #small, this data will be removed, i.e., this data will be
                  #removed from fm.v
                }
                if (isTRUE(FALSE%in%is.na(rmEnd.index))){
                  fm.v<-fm.v[-rmEnd.index[is.na(rmEnd.index)==F],]
                }
                return(fm.v)
              } else {
                rm(fm.v)
              }
            }
          })

        trial=trial+1
        if(isTRUE(nrow(fm.v)>0)){fm.v=fm.v} else { rm(fm.v) }
      }
      if (isTRUE(!exists('fm.v'))){
        break
      } else {
        oneday$flag3.day[oneday$ID%in%fm.v$ID]<-0
      }

    }

    one.day$flag3.day[!one.day$datetime%in%oneday$datetime]<-0

    return(one.day)

  })
  data$flag3.day[
    data$datetime%in%(PAM.day$datetime[PAM.day$flag3.day==0])]<-0
  #remove marked abnormal data
  data<-data[-which(data$flag3.day==0),]
  return(data)
}

#' Filter MONI-PAM data step 3,remove spurious F', Fm' and Y(II) value during the day
#'
#'
#' @usage filter3.day(PAM.data,f3.YII=0.02,f3.Fm=0.1,f3.FmYII=3,save.path,save.file)
#' @param PAM.data a data.table or data.frame MONI-PAM data generated from [filter2.night] function.
#' @param f3.YII the threshold of percentage change of YII between consecutive points. Default value is 0.02, we recommend this argument can be adjusted from 0.01 to 0.1 by an interval of 0.01.
#' @param f3.Fm the threshold of percentage change of Fm' between consecutive points. Default value is 0.1, we recommend this argument can be adjusted from 0.05 to 0.3 by a 0.05 interval by an interval of 0.05.
#' @param f3.FmYII the threshold of ratio between percentage change in Fm' between consecutive points and in Y(II) between consecutive points. Default value is 3, we recommend this argument can be adjust between 2 and 5 by an interval of 1.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder

#' @importFrom plyr ldply
#' @import dplyr
#'
#' @return [filter3.day] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'PAM_Year1_Year2_filter3day.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter2.night] function named as 'flag3.day'. This column only contain two values: 0 and 1, where 0 means F', Fm' and YII in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter3.day<-function(PAM.data,f3.YII=0.02,
                      f3.Fm=0.1,
                      f3.FmYII=3,
                      save.path,
                      save.file)
{
  start.time<-Sys.time()
  print('This function will run around 1 min.')
  PAM.data<-formatPAMdata(PAM.data = PAM.data)

  day.filter<-
    ldply(levels(PAM.data$head_tree),function(i){
      PAM.onetree<- PAM.data[PAM.data$head_tree==i,]
      if (isTRUE(nrow(PAM.onetree)>0)) {#if data exsit
        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        PAM.onetree<-PAM.onetree[order(PAM.onetree$datetime),]
        PAM.onetree$flag3.day<-1
        filter.onetree<-
          filter.o.fc(data=PAM.onetree,f3.Fm=f3.Fm,f3.FmYII=f3.FmYII)
        trials <- 0
        print(paste0(i, ' is filtering...'))
        while(nrow(filter.onetree[filter.onetree$flag3.day==0,])>0){
          filter.onetree<-
            filter.o.fc(data=filter.onetree,f3.Fm=f3.Fm,f3.FmYII=f3.FmYII)
          trials <- trials +1
        }

        filter.onetree2<-filter.pm.fc(data=filter.onetree,f3.YII = f3.YII,f3.Fm=f3.Fm)
        trials <- 0
        while(isTRUE(nrow(filter.onetree2[filter.onetree2$flag3.day==0,])>0)){
          filter.onetree2<-filter.pm.fc(data=filter.onetree2,f3.YII = f3.YII,f3.Fm=f3.Fm)
          trials <- trials +1
        }
        filter.onetree3<-filter.v.fc(data=filter.onetree2)
        PAM.onetree$flag3.day[
          !PAM.onetree$datetime%in%filter.onetree3$datetime]<-0
      }

      return(PAM.onetree)
    })

  if (isTRUE(save.file==T)){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  day.filter[day.filter$flag3.day==0,c('F_','Fm_','YII')]<-NA

  day.filter$flag.all<-
    day.filter$flag1.lowF*day.filter$flag2.night*day.filter$flag3.day

  day.filter<-
    droplevels(day.filter[,c("date", "plot.group","dateBack12h","datetime",
                             "datetimeBack12h","head", "tree_num",
                             "sunrise","sunriseEnd",
                             "solarNoon","sunsetStart",
                             "sunset","dusk",
                             "dawn","F_","Fm_",
                             "YII", "par_PAM","temp_PAM","ETR",
                             'head_tree',"flag1.lowF",
                             'flag2.night','flag3.day',
                             'flag.all')])
  if (isTRUE(save.file==T)){
    write.table(day.filter,file =
                  paste0(save.path,'/PAM_',year(range(day.filter$date)[1]),
                         '_', year(range(day.filter$date)[2]),
                         '_filter3day.dat'),
                row.names = F,sep = ';')

  }

  print(Sys.time()-start.time)
  return(day.filter)
}
