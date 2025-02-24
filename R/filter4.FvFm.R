#' filter logical function 1 for filter4.FvFm function
#' @export
filter.FvFm.fc41<-function(data,f4.fm,f4.fmYII){
  data<-data[data$flag4.FvFm==1,]
  PAM.night<-
    na.omit(data[-which(data$datetime<=data$sunsetStart&
                          data$datetime>=data$sunrise),])
  fvfm<-
    PAM.night%>%
    subset(select=c('dateBack12h','datetime','YII'))%>%
    group_by(dateBack12h)%>%
    filter(YII==max(YII,na.rm = T))%>%
    dplyr::rename(FvFm=YII)
  fm<-
    PAM.night%>%
    subset(select=c('dateBack12h','datetime','Fm_'))%>%
    group_by(dateBack12h)%>%
    filter(Fm_==max(Fm_,na.rm = T))%>%
    dplyr::rename(Fm=Fm_)

  #by merging fvfm and Fm, there will be many repeated dateback12h due to one to
  #several max FvFm or Fm.
  #for the aim of filter, if one is bad point, other same bad value point
  #that have different datetime will also be filtered.
  fvfm<-
    fvfm%>%
    merge(fm,by=c('dateBack12h'),all = T)%>%
    mutate(F0=Fm*(1-FvFm))%>%
    dplyr::rename(datetime.FvFm=datetime.x,datetime.Fm=datetime.y)

  Dfvfm<-
    fvfm%>%
    subset(select=c('dateBack12h','FvFm','Fm','F0'))%>%
    unique%>%
    mutate(DFvFm=c(diff(FvFm),NA)/FvFm,
           DFm=c(diff(Fm),NA)/Fm,
           DF0=c(diff(F0),NA)/F0)%>%
    mutate(DFvFm=c(NA,DFvFm[-length(DFvFm)]),
           DFm=c(NA,DFm[-length(DFm)]),
           DF0=c(NA,DF0[-length(DF0)]),
           flag4.FvFm=1)%>%
    mutate(flag4.FvFm=case_when(
      (DFm<=(-1*f4.fm)&DF0<=0&DFm/DFvFm>=f4.fmYII)|# fm decreases, F0 decreases,
        #and decreases in Fm were much higher then Fv/FM (maxYII) or
        (DFm<=(-0.2)&DFvFm>=0)| #Fm decreases and fv/fm (maxYII) increases or didnot change or
        (DF0<=(-0.3)&DFvFm>=0)| # F0 decrease and fv/fm (maxYII) increases or didnot change or
        (Fm<=100&DFm/DFvFm>1.5&DFm<0)| #Fm<100 and Fm decreases and decreases in Fm were much higher then Fv/FM (maxYII)
        #(DFvFm>=0.4&DF0<0&DFm<DFvFm)| # Fv/Fm (maxYII) increases, and F0 decreases, and percentage increase in Fm
        # were lower than percentage increase in FvFm
        (DFvFm>=0.5&DFvFm/DF0>=2&  # Fv/Fm increases and increase in Fv/Fm were much higher than increase in F0 and Fm<200
           Fm<=200)
      ~0
    ))

  flag.time<-
    c(fvfm$datetime.FvFm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.FvFm==0]],
      fvfm$datetime.Fm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.FvFm==0]])

  data$flag4.FvFm[data$datetime%in%flag.time]<-0

  return(data)
}

#' filter logical function 2 for filter4.FvFm function
#' @export
filter.FvFm.fc42<-function(data,f4.fm,f4.fmYII){
  data<-data[data$flag4.FvFm==1,]
  PAM.night<-
    na.omit(data[-which(data$datetime<=data$sunsetStart&
                          data$datetime>=data$sunrise),])
  fvfm<-
    PAM.night%>%
    subset(select=c('dateBack12h','datetime','YII'))%>%
    group_by(dateBack12h)%>%
    filter(YII==max(YII,na.rm = T))%>%
    dplyr::rename(FvFm=YII)
  fm<-
    PAM.night%>%
    subset(select=c('dateBack12h','datetime','Fm_'))%>%
    group_by(dateBack12h)%>%
    filter(Fm_==max(Fm_,na.rm = T))%>%
    dplyr::rename(Fm=Fm_)

  #by merging fvfm and Fm, there will be many repeated dateback12h due to one to
  #several max FvFm or Fm.
  #for the aim of filter, if one is bad point, other same bad value point
  #that have different datetime will also be filtered.
  fvfm<-
    fvfm%>%
    merge(fm,by=c('dateBack12h'),all = T)%>%
    mutate(F0=Fm*(1-FvFm))%>%
    dplyr::rename(datetime.FvFm=datetime.x,datetime.Fm=datetime.y)

  Dfvfm<-
    fvfm%>%
    subset(select=c('dateBack12h','FvFm','Fm','F0'))%>%
    unique%>%
    mutate(DFvFm=c(diff(FvFm),NA)/FvFm,
           DFm=c(diff(Fm),NA)/Fm,
           DF0=c(diff(F0),NA)/F0)%>%
    mutate(DFvFm=c(NA,DFvFm[-length(DFvFm)]),
           DFm=c(NA,DFm[-length(DFm)]),
           DF0=c(NA,DF0[-length(DF0)]),
           flag4.FvFm=1)%>%
    mutate(flag4.FvFm=case_when(
      (DFvFm<0&DFm>0.5)|
        (DFvFm<0&DF0>0.5)#|
        #(DFvFm>0&DFm>1&DFvFm<DFm)
      ~0
    ))

  flag.time<-
    c(fvfm$datetime.FvFm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.FvFm==0]],
      fvfm$datetime.Fm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.FvFm==0]])

  data$flag4.FvFm[data$datetime%in%flag.time]<-0

  return(data)
}


#' Filter MONI-PAM data step 4,remove abnormal Fv/Fm (or maximum YII) value for entire observation season
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @usage filter4.FvFm(PAM.data,save.path,save.file,f4.fm=0.15,f4.fmYII=3)
#' @param PAM.data a data.table or data.frame MONI-PAM data generated from [filter3.day] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#' @param f4.fm the threshold of percentage change of Fm between day2 and day1. Default value is 0.15, we recommend this argument can be adjusted from 0.05 to 0.3 by an interval of 0.05.
#' @param f4.fmYII the threshold of ratio between percentage change of Fm between day2 and day1 and of FvFm between day2 and day1. Default value is 3, we recommend this argument can be adjust between 2 and 5 by an interval of 1.
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom plyr ldply
#' @import dplyr
#'
#' @return [filter4.FvFm] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'PAM_Year1_Year2_filter4FvFm.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter3.day] function named as 'flag4.FvFm'. This column only contain two values: 0 and 1, where 0 means F', Fm' and YII in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter4.FvFm<-function(PAM.data,
                           save.path,
                           save.file,
                           f4.fm=0.15,
                           f4.fmYII=3
                           ){
  print('This function will run mins...' )

  PAM.data<-formatPAMdata(PAM.data = PAM.data)
  start.time<-Sys.time()
  FvFm.filter<-
    ldply(levels(PAM.data$head_tree),function(i){
      print(paste0(i, ' is filtering...'))
      PAM.onetree<-PAM.data[PAM.data$head_tree==i,]

      if (nrow(PAM.onetree[which((!is.na(PAM.onetree$YII))&
                                  !is.na(PAM.onetree$Fm_)&
                                  !is.na(PAM.onetree$F_)),])>0) {#if data exsit

        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        PAM.onetree<-PAM.onetree[order(PAM.onetree$datetime),]
        PAM.onetree$flag4.FvFm<-1
        filter.onetreeYII1<-
          filter.FvFm.fc41(data=PAM.onetree,f4.fm=f4.fm,
                               f4.fmYII=f4.fmYII)
        trials <- 0
        while(nrow(filter.onetreeYII1[
          filter.onetreeYII1$flag4.FvFm==0,])>0){
          filter.onetreeYII1<-
            filter.FvFm.fc41(filter.onetreeYII1,f4.fm=f4.fm,
                                 f4.fmYII=f4.fmYII)
          trials <- trials +1
        }

        filter.onetreeYII2<-
          filter.FvFm.fc42(data=filter.onetreeYII1,f4.fm=f4.fm,
                               f4.fmYII=f4.fmYII)
        trials <- 0
        while(nrow(filter.onetreeYII2[
          filter.onetreeYII2$flag4.FvFm==0,])>0){
          filter.onetreeYII2<-
            filter.FvFm.fc42(filter.onetreeYII2,f4.fm=f4.fm,
                                 f4.fmYII=f4.fmYII)
          trials <- trials +1
        }


        PAM.onetree$flag4.FvFm[
          !PAM.onetree$datetime%in%filter.onetreeYII2$datetime]<-0
        #if all data during night is filtered, remove whole day
        to.fvfm<-PAM.onetree
        to.fvfm[to.fvfm$flag4.FvFm==0,c('F_','Fm_','YII')]<-NA
        sub.fvfm<-FindFvFm(to.fvfm,save.file = F,
                           save.path = save.path,save.title = '')
        sub.fvfm<-sub.fvfm[is.na(sub.fvfm$Fm),]
        PAM.onetree$flag4.FvFm[
          (PAM.onetree$dateBack12h%in%(sub.fvfm$date-1))&
            (PAM.onetree$flag.all==1)]<-0
      }

      return(PAM.onetree)

    }
    )
  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  FvFm.filter<-data.table(FvFm.filter)
  FvFm.filter[FvFm.filter$flag4.FvFm==0,c('F_','Fm_','YII')]<-NA

  FvFm.filter$flag.all<-
    FvFm.filter$flag.all*FvFm.filter$flag4.FvFm
  FvFm.filter<-
    droplevels(FvFm.filter[,c("date", "plot.group","dateBack12h","datetime",
                                  "datetimeBack12h","head", "tree_num",
                                  "sunrise","sunriseEnd",
                                  "solarNoon","sunsetStart",
                                  "sunset","dusk",
                                  "dawn","F_","Fm_","YII",
                                  "par_PAM","temp_PAM","ETR",'head_tree',
                                  "flag1.lowF.YII",'flag2.night',
                                  'flag3.day',
                                  "flag4.FvFm",'flag.all')])
  if (save.file==T){
    write.table(FvFm.filter,file =
                  paste0(save.path,'/PAM_',year(range(FvFm.filter$date)[1]),
                         '_', year(range(FvFm.filter$date)[2]),
                         '_filter4FvFm.dat'),
                row.names = F,sep = ';')
  }
  print(Sys.time()-start.time)
  return(FvFm.filter)
}
