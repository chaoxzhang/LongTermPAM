#' Filter MONI-PAM data step 6,remove additionally a few more data points adjacent to the previously filtered data
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @usage filter6.adjacent(PAM.data,save.path,save.file,expand.time)
#' @param PAM.data a data.table or data.frame MONI-PAM data generated from [filter5.expand] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#' @param expand.time an integer value.  This value means a time window in minutes and allows users using different sampling intervals to decide how many points adjacent to filtered data they want to remove. For example, for a 20-min MONI-PAM measurement interval dataset, if expand.time=60 (it means 60 mins), and two points of both left and right side of current removed point will be removed from this function.
#'
#' @return [filter6.adjacent] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'PAM_Year1_Year2_filter6adjacent.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter5.expand] function named as 'flag6.adjacent'. This column only contain two values: 0 and 1, where 0 means F', Fm' and YII in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter6.adjacent<-function(PAM.data,
                        save.path,
                        save.file,
                        expand.time){
  start.time<-Sys.time()
  print('This function will around 1 min.')
  PAM.data<-formatPAMdata(PAM.data = PAM.data)
  adjacent.filter<-
    ldply(levels(PAM.data$head_tree),function(i){
      print(paste0(i, ' is filtering...'))
      PAM.onetree<- PAM.data[PAM.data$head_tree==i,]

       if (nrow(PAM.onetree)>0) {#if data exsit

        ##<<- order() to make sure correct time order
        PAM.onetree<-PAM.onetree[order(PAM.onetree$datetime),]
        PAM.onetree$flag6.adjacent<-1
        #expand to adjacent: 1 hour before and after the filtered data
        expand.back<-
          PAM.onetree$datetime[is.na(PAM.onetree$Fm_)]-60*expand.time
        expand.forward<-
          PAM.onetree$datetime[is.na(PAM.onetree$Fm_)]+60*expand.time
        flag.time<-ldply(1:length(expand.back),function(i){

         flag.time<-
           data.frame(datetime=PAM.onetree$datetime[
            PAM.onetree$datetime>=expand.back[i]&
              PAM.onetree$datetime<=expand.forward[i]])
        })

        PAM.onetree$flag6.adjacent[(PAM.onetree$datetime%in%
                                      flag.time$datetime)]<-0
       }

      return(PAM.onetree)
    }
    )
  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  adjacent.filter[adjacent.filter$flag6.adjacent==0,c('F_','Fm_','YII')]<-NA

  adjacent.filter$flag.all<-
    adjacent.filter$flag1.lowF.YII*adjacent.filter$flag2.night*
    adjacent.filter$flag3.day*adjacent.filter$flag4.FvFm*
    adjacent.filter$flag5.expand*adjacent.filter$flag6.adjacent


  adjacent.filter<-
    droplevels(adjacent.filter[,c("date", "plot.group","dateBack12h","datetime",
                               "datetimeBack12h","head", "tree_num",
                               "sunrise","sunriseEnd",
                               "solarNoon","sunsetStart",
                               "sunset","dusk",
                               "dawn","F_","Fm_",
                               "YII", "par_PAM","temp_PAM","ETR",
                               'head_tree',"flag1.lowF.YII",
                               'flag2.night','flag3.day',
                               'flag4.FvFm',
                               'flag5.expand','flag6.adjacent','flag.all')])
  if (save.file==T){
    write.table(adjacent.filter,file =
                  paste0(save.path,'/PAM_',year(range(adjacent.filter$date)[1]),
                         '_', year(range(adjacent.filter$date)[2]),
                         '_filter6adjacent.dat'),
                row.names = F,sep = ';')

  }
  print(Sys.time()-start.time)
  return(adjacent.filter)
}
