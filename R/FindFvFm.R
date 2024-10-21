#' Retrieve Fv/Fm data from MONI-PAM data
#'
#' This function will get maximum values of quantum YII of photosystem II (maximum YII) for each observing night (highest YII between sunset of Day i and sunrise of Day i+1) for each MONI-head. We use these maximum YII to represent FV/FM because during the night time, the PAR is close to 0 and almost all photosynthetic reaction center should be open for hours.  In principle, the maximum YII should be observed during the dark period when the PAR is close to 0 and almost all photosynthetic reaction centers should be closed for hours. This is the reason that we can use the maximum YII from nighttime to represent FV/FM in continuous MONI-PAM measurement
#'
#' @usage FindFvFm(PAM.data,save.path,save.title, save.file)
#' @param PAM.data a combined organized MONI-PAM data which is generated from [readPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as filter1.NA, filter2.night and so on.
#' @param save.path local folder for saving your output data
#' @param save.title any text that will be used as the saved file name
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if False, the file will not be saved into local folder
#'
#' @return [FindFvFm] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a '.dat' file.
#' @export
FindFvFm<-function(PAM.data,save.path,save.title, save.file){

  ##--> the output data includes FvFm, and the corresponding F',Fm',
  ##--> temperature,and PAR when YII is maximum (i.e., when FvFm is selected)

  PAM.data<-formatPAMdata(PAM.data = PAM.data)
  noonmean.PAR<-
    PAM.data %>%
    subset(datetime>=as.POSIXct(paste(date,'10:00:00'))&
             datetime<=as.POSIXct(paste(date,'15:00:00'))) %>%
    group_by(head,tree_num,head_tree,date) %>%
    dplyr::summarise(noonmean_PAR=if (all(is.na(par_PAM))) NA else mean(par_PAM, na.rm = TRUE),.groups = "drop")

  PAM.night<-
    PAM.data %>%
    subset(datetime>sunsetStart|datetime<sunrise) %>%
    dplyr::select(dateBack12h,head_tree,head,tree_num,par_PAM,
                  temp_PAM,Fm_,YII) %>%
    dplyr::rename(date=dateBack12h) %>%
    mutate(date=date+1) %>%
    mutate(YII=case_when(
      YII<0|YII>=1~NA,
      TRUE~YII
    ))

  fvfm<-
    PAM.night %>%
    group_by(date,head_tree,head,tree_num) %>%
    dplyr::summarise(FvFm=if (all(is.na(YII))) NA else max(YII, na.rm = TRUE),
                     Fm=if (all(is.na(Fm_))) NA else max(Fm_, na.rm = TRUE),
                     nightmean_temp=if (all(is.na(temp_PAM))) NA else mean(temp_PAM, na.rm = TRUE),
                     .groups = 'drop') %>%
    mutate(F0=(1-FvFm)*Fm)

  range.date<-
    fvfm %>%
    dplyr::select(head_tree,date) %>%
    group_by(head_tree) %>%
    mutate(start.date=min(date,na.rm = T),end.date=max(date,na.rm = T)) %>%
    dplyr::select(-date) %>%
    unique()


  fvfm.filldate<-
    ldply(1:nrow(range.date),function(i){

      range.i<-range.date[i,]
      date.seq<-seq(lubridate::date(range.i$start.date),
                    lubridate::date(range.i$end.date),by='day')
      fvfm.i<-fvfm %>% subset(head_tree==range.i$head_tree)
      #fvfm[fvfm$head_tree==range.date$head_tree[i],]
      fill.dategap<-complete(fvfm.i,date=date.seq,
                             nesting(head_tree,head,tree_num))
      return(fill.dategap)
    })

  fvfm.all<-fvfm.filldate %>%
    merge(noonmean.PAR,all=T,by=c('date','head_tree','head','tree_num')) %>%
    select(date,head_tree,head,tree_num,noonmean_PAR,nightmean_temp,
           F0,Fm,FvFm)


  if (save.file==T) {
    write.table(fvfm.all,file =
                  paste0(save.path,'/PAM_FvFm_',save.title,'_',
                         lubridate::year(range(fvfm.all$date)[1]),'_',
                         lubridate::year(range(fvfm.all$date)[2]),
                         '.dat'),
                row.names = F,sep = ';')
  }

  return(data.table(fvfm.all))

}
