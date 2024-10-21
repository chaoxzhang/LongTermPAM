#' obtain extreme values of MONI-PAM data
#'
#' These extreme values of MONI-PAM data will be used for MONI-PAM data visualization
#'
#' @usage scalePAMdata(PAM.data)
#' @param PAM.data a combined organized MONI-PAM data which is generated from [readPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.NA], [filter2.night] and so on.
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#'
#' @return [scalePAMdata] will return a data.table which contains the maximum and minimum values of F',FM',YII, PAR, and temperature values for each plotting groups (every 10 days as a group) and each MONI-head
#' @export
scalePAMdata<-function(PAM.data){

  PAM.data<-formatPAMdata(PAM.data=PAM.data)
  data<-PAM.data %>%
    dplyr::select(plot.group,head,tree_num,head_tree,
                  F_,Fm_,YII,par_PAM,temp_PAM)

  # Define custom functions to handle Inf values
  safe_max <- function(x) {
    result <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.infinite(result)) NA else result
  }

  safe_min <- function(x) {
    result <- suppressWarnings(min(x, na.rm = TRUE))
    if (is.infinite(result)) NA else result
  }

  maxmin.res<-
    data %>%
    group_by(plot.group,head,tree_num,head_tree) %>%
    dplyr::summarise(
      across(
        .col=c('F_','Fm_','YII','par_PAM','temp_PAM'),
        .fns = list(max = safe_max, min = safe_min),
        .names = "{.fn}.{.col}"
      ),
      .groups = 'drop'
    )
  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res<-
    maxmin.res %>%
    mutate(scale.Fm.a=(max.YII-min.YII)/(max.Fm_-min.Fm_)) %>%
    mutate(scale.Fm.b=min.YII-scale.Fm.a*min.Fm_) %>%
    mutate(scale.temp.a=(max.YII-min.YII)/(max.temp_PAM-min.temp_PAM)) %>%
    mutate(scale.temp.b=min.YII-scale.temp.a*min.temp_PAM) %>%
    mutate(scale.par.a=(max.YII-min.YII)/(max.par_PAM-min.par_PAM)) %>%
    mutate(scale.par.b=min.YII-scale.par.a*min.par_PAM)


  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res[sapply(maxmin.res, is.nan)]<-NA
  maxmin.res[,c(15,17,19)][sapply(maxmin.res[,c(15,17,19)],is.na)]<-1
  maxmin.res[,c(16,18,20)][sapply(maxmin.res[,c(16,18,20)],is.na)]<-0
  return(maxmin.res)
}
