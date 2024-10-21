#' Estimate seasonal chlorophyll fluorescence parameters
#'
#' This function will estimate seasonal parameters,including PQs, qLs, and NPQs. Please see calculation method and parameter description in Porcar-Castell et al. 2011 and Zhang et al. XX.
#'
#' @usage seasonalParams(filtered.fvfm,FmR.data,save.file,save.path)
#' @param raw.fvfm MONI-PAM fvfm data retrieved from original data (i.e., flag0.fvfm in Intro_to_processMONIPAM.Rmd) by using [FindFvFm] function in this R package
#' @param filter.fvfm the final MONI-PAM fvfm data retrieved from cleaning step 6 (i.e., flag6.fvfm in Intro_to_processMONIPAM.Rmd) by using [FindFvFm] function in this R package
#' @param save.file TRUE or FALSE. If TRUE, output data will be saved to local folder via save.path argument
#' @param save.path local folder for saving the plotted figures generated from this function
#' @references reference Porcar‐Castell, A. (2011). A high‐resolution portrait of the annual dynamics of photochemical and non‐photochemical quenching in needles of Pinus sylvestris. Physiologia Plantarum, 143(2), 139-153. https://doi.org/10.1111/j.1399-3054.2011.01488.x
#' @return This function will return a data frame including new estimated seasonal parameters and Fv/Fm data
#' @export
seasonalParams<-function(filtered.fvfm,
                         FmR.data,
                         save.file,
                         save.path){
  seasonal.all<-data.table(filtered.fvfm)

  seasonal.all<-
    merge(seasonal.all,FmR.data %>% select(season,head_tree,head,tree_num,FmR,FoR),
          by=c('head_tree','head','tree_num'),all=T)
  seasonal.all<-
    seasonal.all %>%
    mutate(PQs=FmR/F0-FmR/Fm,
           #fDs=F0/FmR,
           qLs=(1/F0-1/Fm)/(1/FoR-1/FmR),
           NPQs=FmR/Fm-1) %>%
    dplyr::select(season,date,head_tree,head,tree_num,noonmean_PAR,nightmean_temp,
                  F0,Fm,FvFm,FmR,FoR,PQs,qLs,NPQs)

  if (save.file==T){
    write.table(seasonal.all,row.names = F,sep=';',
                file=paste0(save.path,'/',unique(seasonal.all$season),
                            '_','seasonal_parameters','.csv'))
  }
  return(seasonal.all)
}
