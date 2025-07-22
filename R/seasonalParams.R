#' Estimate seasonal chlorophyll fluorescence parameters
#'
#' This function estimates seasonal parameters,including quenching parameters PQs, qLs, and NPQs, and yield parameters Phi_NPQs and Phi_fDs.
#'
#' @usage seasonalParams(filtered.fvfm,FmR.data,save.file,save.path)
#' @param fvfm.afterFilter the final MONI-PAM fvfm data retrieved from cleaning step 6 (i.e., flag6.fvfm in Intro_to_processMONIPAM.Rmd) by using [FindFvFm] function in this R package
#' @param FmR.data FmR and F0R data can be retrieved from the [ChlFRef] function or defined manually by the user.  This data should be a data frame or data table and include columns 'season','head_tree','head','tree_num','FmR' and 'F0R'
#' @param save.file TRUE or FALSE. If TRUE, output data will be saved to local folder via save.path argument
#' @param save.path local folder for saving the output data generated from this function
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @return This function will return a data frame including new estimated seasonal parameters and Fv/Fm data
#' @export
seasonalParams<-function(fvfm.afterFilter,
                         FmR.data,
                         save.file,
                         save.path){
  seasonal.all<-data.table(fvfm.afterFilter)

  seasonal.all<-
    merge(seasonal.all,
          FmR.data %>%
            # only the FmR and F0R estimated from final filtered
            # data will be used to calculate ChlF parameters
            subset(var=='After filter') %>%
            select(season,head_tree,head,tree_num,FmR,F0R) %>%
            droplevels(),
          by=c('head_tree','head','tree_num'),all=T)
  seasonal.all<-
    seasonal.all %>%
    mutate(PQs=FmR/F0-FmR/Fm,
           qLs=(1/F0-1/Fm)/(1/F0R-1/FmR),
           NPQs=FmR/Fm-1,
           Phi_NPQs=F0/Fm-F0/FmR,
           Phi_fDs=F0/FmR) %>%
    dplyr::select(season,date,head_tree,head,tree_num,noonmean_PAR,nightmean_temp,
                  F0,Fm,FvFm,FmR,F0R,PQs,qLs,NPQs,Phi_NPQs,Phi_fDs)

  if (save.file==T){
    write.table(seasonal.all,row.names = F,sep=';',
                file=paste0(save.path,'/',unique(seasonal.all$season),
                            '_','seasonal_parameters','.csv'))
  }
  return(seasonal.all)
}
