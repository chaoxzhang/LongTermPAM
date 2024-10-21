#' Estimate diurnal chlorophyll fluorescence parameters
#'
#' This function will estimate diurnal parameters,including rate constant parameters: PQ, NPQ,NPQr; quenching parameters: qLT,qLr, and quantum yield parameters: Phi_NPQ, Phi_NPQr,Phi_NPQs, Phi_fD. Please see calculation method and parameter description in Porcar-Castell et al. 2011 and Zhang et al. XX
#'
#' @usage diurnalParams(filtered.data,FmR.data,save.file,save.path)
#' @param filtered.data the final filtered MONI-PAM data retrieved from cleaning step 6  (i.e., flag6.neighbor in Intro_to_LongTermPAM.Rmd)
#' @param FmR.data estimated chlorophyll fluorescence reference data, which can be retrieved from [ChlFRef] function in this R package, or prepared by yourself. This data should be a data frame or data table and include columns 'season','head_tree','head','tree_num','FmR' and 'FoR'
#' @param save.file TRUE or FALSE. If TRUE, output data will be saved to local folder via save.path argument
#' @param save.path local folder for saving the output data generated from this function
#'
#' @import lubridate
#' @import data.table
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
    dplyr::select(season,head_tree,head,tree_num,FmR,FoR) %>%
    mutate(across(c(season,head,head_tree,tree_num),as.factor))

  diurnal.all<-
    diurnal.data %>%
    merge(filtered.fvfm,by=c('date','head_tree','head','tree_num'),all=T) %>%
    merge(FmR.data,by=c('head_tree','head','tree_num'),all=T)

  diurnal.all<-
    diurnal.all %>%
    mutate(
      # calculate rate constant parameters
      PQ=FmR/F_-FmR/Fm_,
      NPQ=FmR/Fm_-1,
      NPQr=FmR/Fm_-FmR/Fm,
      # calculate quenching parameters
      qLT=(1/F_-1/Fm_)/(1/FoR-1/FmR),
      qLr=(1/F_-1/Fm_)/(1/F0-1/Fm),
      # calculate quantum yield parameters
      Phi_NPQ=F_/Fm_-F_/FmR,
      Phi_NPQr=F_/Fm_-F_/Fm,
      Phi_NPQs=F_/Fm-F_/FmR,
      Phi_fD=F_/FmR, #SAME TREND WITH F_, SINCE FMR IS CONSTANT
      #recalculate ETR
      ETR=0.84*par_PAM*YII*0.5
    ) %>%
    dplyr::select(season,plot.group, date,   dateBack12h,  datetime,  datetimeBack12h,
                  sunrise, sunriseEnd, solarNoon, sunsetStart, sunset, dusk, dawn,
                  head_tree, head, tree_num, flag.all,par_PAM, temp_PAM,
                  F_, Fm_, YII, ETR,  F0, Fm, FvFm, FmR, FoR, PQ, NPQ, NPQr,
                  qLT, qLr, Phi_NPQ, Phi_NPQr, Phi_NPQs, Phi_fD)

  if (save.file==T){
    write.table(diurnal.all,row.names = F,sep=';',
                file=paste0(save.path,'/',unique(diurnal.all$season),
                            '_','diurnal_parameters','.csv'))

  }

  return(diurnal.all)

}
