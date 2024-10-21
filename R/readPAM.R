#' organize data function used only for readPAM function
#' @export
organize.PAMdata<-function(data,shortnames,i){

  data$date<-ymd(data$YYMMDD)
  data$HHMMSS<-str_sub(data$HHMMSS,start = 1,end = 8)
  data$datetime<-paste(data$date,data$HHMMSS)
  data$filename<-shortnames[i]

  data$tree_num<-gsub(pattern = ' ',replacement = '',data$tree_num)
  data$tree_num<-as.factor(data$tree_num)
  #add "^" before and "$" after "-" for F_ and Fm_,
  # to make sure only "-" is replaced and the
  # negative values will not be deleted
  data$F_<-gsub(pattern = '^-$',replacement = NA,data$F_)
  data$F_<-suppressWarnings(as.numeric(data$F_))

  data$Fm_<-gsub(pattern = '^-$',replacement = NA,data$Fm_)
  data$Fm_<-suppressWarnings(as.numeric(data$Fm_))

  data$YII<-gsub(pattern = '^-$',replacement = NA,data$YII)
  data$YII<-suppressWarnings(as.numeric(data$YII))

  data$par_PAM<-gsub(pattern = ' ',replacement = '',data$par_PAM)
  data$par_PAM<-suppressWarnings(as.numeric(data$par_PAM))

  data$ETR<-gsub(pattern = '-',replacement = NA,data$ETR)
  data$ETR<-suppressWarnings(as.numeric(data$ETR))

  data$temp_PAM<-gsub(pattern = ' ',replacement = '',data$temp_PAM)
  data$temp_PAM<-suppressWarnings(as.numeric(data$temp_PAM))
  return(data)

}

#' read and organize  MONI-PAM data
#'
#' The output of this function will include many files that will be saved into
#' local folder, including
#' (1) rawEachFilename_Year.csv: organized each single file.
#' (2) four information files: rawFile_noHeadInfo.txt (if no head information recorded),
#' rawFile_newDeviceAdded.txt (if new device(s) added),
#'  rawFile_noTreeDefined.txt (if tree_num is not defined in comment column),
#'   and rawFile_noDataRecord.txt (if no data is recorded). These files will be
#'  saved into local folder if the according condition occurs, then you need
#'  to mordify your MONI-PAM files based on error or warning message in
#'  order to continue smoothly organize your data and further data cleaning.
#' (3) "preprocesPAM_head_timerange_Year1_Year2.dat": a summary of head,
#'  tree-number and measuring time information.
#' (4) "preprocesPAM_Year1_Year2.dat": combined all the
#'  organized data and this is the file used for further data
#'  processing and cleaning. In this file, columns "plot.group",
#'  "dateBack12h",datetimeBack12h","sunrise","sunriseEnd","solarNoon",
#'  "sunsetStart", "sunset", and "dusk" were added for later easily
#'  data cleaning and data visualization purpose.
#'
#' @usage readPAM(source.path,pam.pattern,save.path,site.lat,site.lon,local.tz,tz.summer,tz.winter,measure.time)
#' @param source.path local path where original MONI-PAM data are stored
#' @param pam.pattern MONI-PAM data extension, '.pam' or '.PAM'
#' @param save.path local path where the exported data will be saved
#' @param site.lat latitude of the study site
#' @param site.lon longitude of the study site
#' @param local.tz time zone for the study site, for example, it is 'EET' for Finland
#' @param tz.summer UTC offset of study site in summer, for example, it is 3 for Finland
#' @param tz.winter UTC offset of study site in winter, for example, it is 2 for Finland
#' @param measure.time which time zone used for measurement: 'winter','summer' or 'local'
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @import stringr
#' @import suncalc
#' @importFrom data.table data.table setDT
#' @import splitstackshape
#' @import dplyr
#' @importFrom plyr ldply
#' @importFrom tidyr complete nesting
#'
#' @return list 1: combined original data; list 2: combined organized data and extra columns for further data cleaning purpose
#'
#' @export
readPAM<-function(source.path,#folder where files stored
                       pam.pattern,#PAM pattern ('.pam' or '.PAM')
                       save.path,#folder to save the exported data
                       site.lat,#latitude of the study site
                       site.lon,#longitude of the study site
                       local.tz,#time zone for the study site
                       tz.summer,# UTC offset of study site in summer
                       tz.winter,# UTC offset of study site in winter
                       measure.time# which time zone used for measurement: winter,summer or local
){

  filenames<-list.files(path = source.path,pattern=pam.pattern,full.names = T)
  shortnames<-gsub(list.files(path =source.path,pattern=pam.pattern),
                   pattern = pam.pattern,replacement = '')

  raw.PAM<-
    ldply(1:length(filenames),function(fn){

      raw<-read.delim(filenames[fn],header = F)
      raw$ID<-1:nrow(raw)
      raw$V1<-iconv(raw$V1, to = "UTF-8", sub = "byte")
      head.count<-nrow(raw[grep('Device|device',raw$V1),])
      if (isTRUE(head.count==0)){
        #when no head information was recorded
        nohead<-
          paste(filenames[fn],' is read into the file but this file did not
          include header information, please check the file and add the
                head information yourself!')
        print(nohead)
        write.table(nohead,append = T,col.names = F,row.names = F,
                    file = paste0(save.path,'/rawFile_noHeadInfo.txt'))
      } else { # when there is a head information, continue following lines
        data.count<-nrow(raw[grep('SG',raw$V1),])
        newdevice.count<-nrow(raw[grep('New',raw$V1),])
        treenum.info<-length(cSplit(raw[grep('Device|device',raw$V1),],
                                    'V1',sep=';',type.convert = F))
        head.num.count<-nrow(unique(cSplit(raw[grep('Device|device',raw$V1),],
                                           'V1',sep = ';',type.convert = F)[,5]))
        tree.num.count<-nrow(unique(cSplit(raw[grep('Device|device',raw$V1),],
                                           'V1',sep = ';',type.convert = F)[,8]))
      }

      if (isTRUE(head.count==0)){#when no head information
        #was recorded
        raw.split.col<-cSplit(raw,'V1', sep = ';',type.convert = F)
        raw.remain<-raw.split.col[,c(1:3,6,8:13)]
        names(raw.remain)<-c('ID','YYMMDD','HHMMSS','head','F_','Fm_',
                             'par_PAM','YII','ETR','temp_PAM')
        raw.head<-raw.head[!raw.head$head==0,]
        raw.remain$head<-as.factor(raw.remain$head)
        raw.remain$tree_num<-NA
        raw.remain<-organize.PAMdata(raw.remain,shortnames,i=fn)
        write.table(raw.remain,row.names = F,sep = ';',
                    file = paste0(save.path,'/',shortnames[fn],
                                  '_',year(raw.remain$date)[1],'.csv'))
        print(paste0(shortnames[fn],' is saved.'))
        return(raw.remain)

      } else if (isTRUE(data.count==0)) {#when no data was recorded
        nodata<-paste0(filenames[fn],' did not record the data!')
        print(nodata)
        write.table(nodata,append = T,col.names = F,row.names = F,
                    file = paste0(save.path,'/rawFile_noDataRecord.txt'))
      } else if (isTRUE(newdevice.count>=1)) {#when new device was added
        raw.newhead<-raw[grep('New',raw$V1),]
        newhead<-
          paste(filenames[fn],' is not read into the file because new device(s)
                was (were) added. ','Please open,check and rearrange the file!')
        print(newhead);print(as.character(raw.newhead[[1]]))
        write.table(newhead,append = T,col.names = F,row.names = F,
                    file = paste0(save.path,'/rawFile_newDeviceAdded.txt'))
      } else if (isTRUE(treenum.info==7)) {
        #when no tree_num information was recorded
        treeNoDefine <-
          paste0(filenames[fn],
                 ' is read into the file but this file did not define tree_num
          information,please check the file and add the tree_num
          information yourself!')
        print(treeNoDefine)
        write.table(treeNoDefine,append = T,col.names = F,row.names = F,
                    file=paste0(save.path,'/rawFile_noTreeDefined.txt'))

        raw.head.1col<-raw[grep('Device|device',raw$V1),]
        raw.1col<-raw[grep('SG',raw$V1),]
        raw.split.col<-cSplit(raw.1col,'V1', sep = ';',type.convert = F)
        raw.remain<-raw.split.col[,c(1:3,6,8:13)]
        names(raw.remain)<-c('ID','YYMMDD','HHMMSS','head','F_','Fm_',
                             'par_PAM','YII','ETR','temp_PAM')
        raw.remain$head<-as.factor(raw.remain$head)
        raw.remain$tree_num<-NA
        raw.remain<-organize.PAMdata(raw.remain,shortnames,i=fn)
        write.table(raw.remain,row.names = F,sep = ';',
                    file = paste0(save.path,'/',shortnames[fn],
                                  '_',year(raw.remain$date)[1],'.csv'))
        print(paste0(shortnames[fn],' is saved.'))
        return(raw.remain)


      } else if (isTRUE(head.num.count<tree.num.count)){
        #when there is less head_number [column 5] than tree_number[column 8],
        #which means the new head(device) is added
        error<-paste0(filenames[fn],' is not read into the file
                because one head has more than one tree_num.
                              Please open,check and rearrange file!')
        print(error)
        write.table(error,file = paste0(save.path,'/rawFile_error.txt'),
                    append = T,col.names = F,row.names = F)
      } else {
        raw.head.1col<-raw[grep('Device|device',raw$V1),]
        raw.head<-cSplit(raw.head.1col,'V1', sep = ';',type.convert = F)
        names(raw.head)<-c('ID','YYMMDD','HHMMSS','PARAMETER',
                          'head','V2','DEVICE','tree_num')
        raw.head<-raw.head[!raw.head$head==0,]
        raw.head$head<-as.factor(raw.head$head)
        raw.head$tree_num<-as.factor(raw.head$tree_num)
        raw.1col<-raw[grep('SG',raw$V1),]
        raw.split.col<-cSplit(raw.1col,'V1', sep = ';',type.convert = F)
        raw.split.col<-raw.split.col[,c(1:3,6,8:13)]
        names(raw.split.col)<-c('ID','YYMMDD','HHMMSS','head','F_','Fm_',
                                'par_PAM','YII','ETR','temp_PAM')
        raw.split.col$head<-as.factor(raw.split.col$head)
        #in some files, the device was reconnected and tree_num was set
        #as same one as previous one, so duplicated head&tree_num can be recorded
        raw.remain<-merge(raw.split.col,unique(raw.head[,c('head','tree_num')]),
                      by='head')
        raw.remain<-organize.PAMdata(raw.remain,shortnames,i=fn)
        write.table(raw.remain,file = paste0(save.path,'/',shortnames[fn],
                                         '_',year(raw.remain$date)[1],'.csv'),
                    row.names = F,sep = ';')
        print(paste0(shortnames[fn],' is saved.'))
        return(raw.remain)

      }

    })

  ##<<- remove the duplicated rows in the raw data
  raw.PAM.unique.all<-data.table(unique(raw.PAM))
  raw.PAM.unique.all<-
    droplevels(raw.PAM.unique.all[,c("filename","YYMMDD","date",
                                      "HHMMSS","datetime", "head","tree_num",
                                      "F_", "Fm_", "YII",
                                      "par_PAM","temp_PAM","ETR" )])
  # save the combined original data to local folder
  write.table(droplevels(raw.PAM.unique.all),
              file = paste0(save.path,'/rawPAM_',
                            range(year(raw.PAM.unique.all$date))[1],'_',
                            range(year(raw.PAM.unique.all$date))[2],'.dat'),
              row.names = F,sep = ';')

  print(paste0('rawPAM_',
               range(year(raw.PAM.unique.all$date))[1],'_',
               range(year(raw.PAM.unique.all$date))[2],'.dat',
               ' [combined raw MONI-PAM files] is saved'))



  ##<<- add additional information for filtering and processing data

  ##<<- 1. remove the duplicated rows

  #remove the duplicates when filename is not considered
  preproces.PAM<-raw.PAM.unique.all %>%dplyr::select(-filename) %>% unique()

  #now maybe there are still duplicated rows if the tree_num is not considered.
  #Since sometime a period can be saved more than one time in different files,
  #but the tree_num may not be assigned a name in one of these files,
  #For example, tree_num is such as 'Tree 5 low' in one file,
  #but is 'NA' in another file when all the other variables are duplicated saved.
  #Therefore, the following code is used to remove these duplicated rows
  preproces.dup<-
    preproces.PAM[duplicated(preproces.PAM%>% dplyr::select(-tree_num),fromLast = T)|
                     duplicated(preproces.PAM%>% dplyr::select(-tree_num)),]

  if (isTRUE(nrow(preproces.dup)>0)){#when without considering 'tree_num' column,
    #the duplicated rows are exist

    preproces.nodup<-
      preproces.PAM[-which(duplicated(preproces.PAM %>% dplyr::select(-tree_num),
                                       fromLast = T)|
                              duplicated(preproces.PAM %>% dplyr::select(-tree_num))),]

    preproces.uniq<-
      rbind(unique(preproces.dup[!is.na(preproces.dup$tree_num),]),
            preproces.nodup)
  } else {#when without considering 'tree_num' column,
    #the duplicated rows are not exist
    preproces.uniq<-preproces.PAM
  }



  #<<- 2. get the observation time range for each head and tree_num
  preproces.uniq$datetime<-ymd_hms(preproces.uniq$datetime)
  range.date.data<-preproces.uniq %>% dplyr::select(datetime,head,tree_num) %>% as.data.table()
  range.date<-
    range.date.data %>%
    group_by(head,tree_num) %>%
    plyr::summarise(start.date=min(datetime,na.rm = T),end.date=max(datetime,na.rm = T))


  write.table(range.date %>% dplyr::select(head,tree_num,start.date,end.date),file =
                paste0(save.path,'/preprocesPAM_head_timerange_',
                       range(year(preproces.uniq$date))[1],'_',
                       range(year(preproces.uniq$date))[2],'.dat'),
               row.names = F,sep = ';')
  print(paste0('preprocesPAM_head_timerange_',
               range(lubridate::year(preproces.uniq$date))[1],'_',
               range(lubridate::year(preproces.uniq$date))[2],'.dat',
               ' [head and tree_num information] is saved'))

  ##<<- 3. fill the time gap for each head and tree_num

  preproces.filldate<-
    ldply(1:nrow(range.date),function(i){

      range.i<-range.date[i,]
      time=seq(ymd_hms(paste(lubridate::date(range.i$start.date),
                             lubridate::hour(range.i$start.date),':00:00')),
               ymd_hms(paste(lubridate::date(range.i$end.date),
                             lubridate::hour(range.i$end.date),':00:00')),
               by='hour')

      if (is.na(range.i$tree_num)){

        fill.data<-
          preproces.uniq %>%
          subset(head==range.i$head&is.na(tree_num)) %>%
          dplyr::select(-YYMMDD)


      } else {

        fill.data<-
          preproces.uniq %>%
          subset(head==range.i$head&tree_num==range.i$tree_num) %>%
          dplyr::select(-YYMMDD)

        }

      #add a column of Year month day and hour,
      #later this will be the filled column
      fill.data$ymdh<-ymd_hms(paste0(lubridate::date(fill.data$datetime),
                                     lubridate::hour(fill.data$datetime),':00:00'))
      #complete from tidyr package
      fill.dategap<-
        complete(fill.data,ymdh=time,nesting(head,tree_num))
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

  ##<<- 4. add the datetime (datetimeBack12h) and
  ##<<-    date (dateBack12h) when move back 12 hours
  ##<<-    for later easily data filter/cleaning and visualization purpose
  preproces.filldate$datetimeBack12h<- preproces.filldate$datetime-12*3600
  preproces.filldate$dateBack12h<-ymd(lubridate::date(preproces.filldate$datetimeBack12h))
  preproces.filldate<- preproces.filldate[order(preproces.filldate$datetime),]

  ##<<- 5. add sunlight information into the file
  ##<<-    for later easily data filter/cleaning and visualization purpose
  #getSunlightTimes from suncalc package
  suntime<-
    getSunlightTimes(date = seq(range(preproces.filldate$date)[1],
                                range(preproces.filldate$date)[2],
                                by='day'),
                     lat=site.lat,lon=site.lon,tz=local.tz)

  namelist<-c('sunrise','sunriseEnd','goldenHourEnd','solarNoon',
              'goldenHour','sunsetStart','sunset','dusk','nauticalDusk',
              'night','nadir','nightEnd','nauticalDawn','dawn')
  namelist.new<-paste0(namelist,'.new')

  #use such as sunrise to get the UTC offset hour(s)
  suntime$tz.num<-ymd_hms(suntime$sunrise,tz='UTC')-suntime$sunrise
  suntime$tz.num<-suppressWarnings(as.numeric(suntime$tz.num))
  suntime[,19:32]<-NA
  names(suntime)[19:32]<-namelist.new
  for (i in 1:14){

    suntime[,namelist.new[i]]<-suntime[,namelist[i]]
    suntime[,namelist.new[i]]<-
      ymd_hms(suntime[,namelist.new[i]],tz='UTC')

    #convert winter sunlight time to summer time
    if(isTRUE(measure.time=='summer')){
      suntime[,namelist.new[i]][which((!is.na(suntime[namelist.new[i]]))&
                                        (suntime$tz.num==tz.winter))]<-
        suntime[,namelist.new[i]][which((!is.na(suntime[namelist.new[i]]))&
                                          (suntime$tz.num==tz.winter))]+3600
      #convert summer sunlight time to winter time
    } else if(isTRUE(measure.time=='winter')){
      suntime[,namelist.new[i]][which((!is.na(suntime[namelist.new[i]]))&
                                        (suntime$tz.num==tz.summer))]<-
        suntime[,namelist.new[i]][which((!is.na(suntime[namelist.new[i]]))&
                                          (suntime$tz.num==tz.summer))]-3600
    } else if(isTRUE(measure.time=='local')){
      suntime[,namelist.new[i]]<-suntime[,namelist.new[i]]
    }

  }

  #when there is no dusk time, using the sunsetStart time - 30min to complement it.
  if (isTRUE(TRUE%in%is.na(suntime$sunrise.new))){
    suntime$sunrise.new[is.na(suntime$sunrise.new)]<-
      ymd_hms(paste0(suntime$date[is.na(suntime$sunrise.new)],'00:00:00'))
    suntime$sunsetStart.new[is.na(suntime$sunsetStart.new)]<-
      ymd_hms(paste0(suntime$date[is.na(suntime$sunsetStart.new)],'23:59:59'))
  }

  #when there is no dawn time, using the sunrise time  to complement it.
  suntime$dawn.new[is.na(suntime$dawn.new)]<-
    suntime$sunrise.new[is.na(suntime$dawn.new)]
  suntime$dusk.new[is.na(suntime$dusk.new)]<-
    suntime$sunsetStart.new[is.na(suntime$dusk.new)]

  preproces.filldate<-
    merge(preproces.filldate,suntime[,c(19:32,1)],
          by='date',all.x = T)
  names(preproces.filldate)<-
    gsub(names(preproces.filldate),pattern='.new',replacement='')

  ##<<- 6. add a group for easy plotting by 10 days,
  ##<<-    the group will be 'Year-Month-a|b|c'
  preproces.filldate$groups<-NA
  preproces.filldate$groups[day(preproces.filldate$datetime)<=10&
                              day(preproces.filldate$datetime)>0]<-'a'
  preproces.filldate$groups[day(preproces.filldate$datetime)<=20&
                              day(preproces.filldate$datetime)>10]<-'b'
  preproces.filldate$groups[day(preproces.filldate$datetime)<=31&
                              day(preproces.filldate$datetime)>20]<-'c'
  preproces.filldate$plot.group<-
    paste(year(preproces.filldate$datetime),
          month(preproces.filldate$datetime),
          preproces.filldate$groups,sep = '-')
  preproces.filldate$plot.group<-as.factor(preproces.filldate$plot.group)


  preproces.filldate<-
    droplevels(
      preproces.filldate[
        ,c("date","plot.group","dateBack12h","datetime","datetimeBack12h",
           "head","tree_num", "sunrise","sunriseEnd","solarNoon",
           "sunsetStart", "sunset","dusk","dawn","F_","Fm_","YII",
           "par_PAM","temp_PAM","ETR")])
  # save combined organized data into local folder
  write.table(preproces.filldate,file =
                paste0(save.path,'/preprocesPAM_',
                       range(year(preproces.filldate$date))[1],'_',
                       range(year(preproces.filldate$date))[2],'.dat'),
              row.names = F,sep = ';')
  print(paste0('preprocesPAM_',
               range(year(preproces.filldate$date))[1],'_',
               range(year(preproces.filldate$date))[2],'.dat',
               ' [combined preprocessed files] is saved'))
  #return 2 lists, list 1--raw.PAM.unique.all: combined original data;
   #               list 2--preproces.filldate: combined organized data
    #                       and extra columns for further data cleaning purpose
  return(list(raw.PAM.unique.all,preproces.filldate))
}
