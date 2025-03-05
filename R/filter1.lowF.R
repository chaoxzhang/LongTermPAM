#' Filter MONI-PAM data step 1,remove spurious low F' and Fm' values
#'
#'
#' @usage filter1.lowF(PAM.data,save.path,save.file)
#' @param PAM.data a combined and organized MONI-PAM data which is generated from [readPAM] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#'
#' @return This function will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'PAM_Year1_Year2_filter1NA.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season. This output file will contain two new column named as 'flag1.lowF' and 'flag.all'. These two columns only contain two values: 0 and 1, where 0 means F', Fm' and YII in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset. 'flag1.lowF' is used to mark which data is/are abnormal data from the data filtering function. 'flag.all' is used to mark all the abnormal data from the all data filtering function.
#' @export
filter1.lowF<-function(PAM.data,
                     save.path, save.file){

  PAM.data<-formatPAMdata(PAM.data = PAM.data)

  ##<<- add a column 'flag1.lowF' to flag empty data of YII
  ##<<- firstly all the data of column 'flag1.lowF' will be 1,
  ##<<- since the good/kept data will be flagged as 1
  PAM.data$flag1.lowF<-1


  ##<<- 1. replace F and Fm as NA, when
  PAM.data[which(is.na(PAM.data$F_)|
                    is.na(PAM.data$Fm_)|
                    PAM.data$F_<=10|#if MONI-PAM ChlF is corrected by temperature, F' can be less than 10
                    PAM.data$Fm_<=50),#if MONI-PAM ChlF is corrected by temperature, Fm' can be less than 50
            c('F_','Fm_')]<-NA

  ##<<- 2. when F or Fm is NA, flag 0 in column 'flag1.lowF', and replace
  ##       YII as NA
  PAM.data$flag1.lowF[is.na(PAM.data$F_)|is.na(PAM.data$Fm_)]<-0
  PAM.data[which(PAM.data$flag1.lowF==0),c('F_','Fm_','YII')]<-NA

  ##<<- 3. when Fm <=F and fm>50, we only removed YII but not
  ##       flag the data as 0, since F and Fm data will be kept
  PAM.data$YII[which(PAM.data$Fm_<=PAM.data$F_&
                          PAM.data$Fm_>50)]<-NA

  PAM.data$flag.all<-PAM.data$flag1.lowF
  if (save.file==T){
    write.table(PAM.data,file =
                  paste0(save.path,'/PAM_',year(range(PAM.data$date)[1]),'_',
                         year(range(PAM.data$date)[2]),
                         '_filter1lowF.dat'),
                row.names = F,sep = ';')
  }
  return(PAM.data)
}
