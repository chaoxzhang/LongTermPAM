#' Estimate diurnal chlorophyll fluorescence parameters
#'
#' This function estimated diurnal parameters,including quenching parameters - PQ, qLT,NPQT, and NPQr, and yield parameters - Phi_NPQT, Phi_NPQr, and Phi_fD. Please see calculation method and parameter description in Porcar-Castell et al. 2011 and Zhang et al. XX
#'
#' @usage diurnalParams(filtered.data,FmR.data,save.file,save.path)
#' @param filtered.data the final filtered MONI-PAM data retrieved from data filtering step 6  (i.e., flag6.adjacent in Intro_to_LongTermPAM.Rmd)
#' @param filtered.fvfm the final FV/FM data retrieved from [FindFVFM] function using final filtered data in filter 6 (i.e., flag6.adjacent in Intro_to_LongTermPAM.Rmd). F0 and Fm are used here for ChlF paramters' calculation
#' @param FmR.data FmR and F0R data can be retrieved from the [ChlFRef] function or defined manually by the user.  This data should be a data frame or data table and include columns 'season','head_tree','head','tree_num','FmR' and 'F0R'
#' @param save.file TRUE or FALSE. If TRUE, output data will be saved to local folder via save.path argument
#' @param save.path local folder for saving the output data generated from this function
#'
#' @import dplyr
#'
#' @references reference Porcar‐Castell, A. (2011). A high‐resolution portrait of the annual dynamics of photochemical and non‐photochemical quenching in needles of Pinus sylvestris. Physiologia Plantarum, 143(2), 139-153. https://doi.org/10.1111/j.1399-3054.2011.01488.x
#' @return This function will return a data frame including estimated diurnal parameters and original data
#' @export
diurnalParams<-function(filtered.data,
                        filtered.fvfm,
                        FmR.data,
                        save.file,
                        save.path){
  #organize data structure
  diurnal.data<-formatPAMdata(filtered.data)

  #organize data structure
  filtered.fvfm<-
    filtered.fvfm %>%
    dplyr::select(date,head_tree,head,tree_num,F0,Fm,FvFm) %>%
    mutate(across(c(head,head_tree,tree_num),as.factor)) %>%
    mutate(date=as.Date(date))

  #organize data structure
  FmR.data<-
    FmR.data %>%
    # only the FmR and F0R estimated from final filtered
    # data will be used to calculate ChlF parameters
    subset(var=='After filter') %>%
    dplyr::select(season,head_tree,head,tree_num,FmR,F0R) %>%
    mutate(across(c(season,head,head_tree,tree_num),as.factor)) %>%
    droplevels()


  diurnal.all<-
    diurnal.data %>%
    merge(filtered.fvfm,by=c('date','head_tree','head','tree_num'),all=T) %>%
    merge(FmR.data,by=c('head_tree','head','tree_num'),all=T)

  diurnal.all<-
    diurnal.all %>%
    mutate(
      # calculate quenching parameters - PQ, qLT,NPQT, and NPQr
      PQ=FmR/F_-FmR/Fm_,
      qLT=(1/F_-1/Fm_)/(1/F0R-1/FmR),
      NPQT=FmR/Fm_-1,
      NPQr=FmR/Fm_-FmR/Fm,
      # calculate quantum yield parameters - Phi_NPQT, Phi_NPQr, and Phi_fD
      Phi_NPQT=F_/Fm_-F_/FmR,
      Phi_NPQr=F_/Fm_-F_/Fm,
      Phi_fD=F_/FmR, #SAME TREND WITH F_, SINCE FMR IS CONSTANT
      #recalculate ETR
      ETR=0.84*par_PAM*YII*0.5
    ) %>%
    dplyr::select(season,plot.group, date,   dateBack12h,  datetime,  datetimeBack12h,
                  sunrise, sunriseEnd, solarNoon, sunsetStart, sunset, dusk, dawn,
                  head_tree, head, tree_num, flag.all,par_PAM, temp_PAM,
                  F_, Fm_, YII, ETR,  F0, Fm, FvFm, FmR, F0R,
                  PQ, qLT, NPQT, NPQr,
                  Phi_NPQT, Phi_NPQr, Phi_fD)

  if (save.file==T){
    write.table(diurnal.all,row.names = F,sep=';',
                file=paste0(save.path,'/',unique(diurnal.all$season),
                            '_','diurnal_parameters','.csv'))

  }

  return(diurnal.all)

}
