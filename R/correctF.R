#' Temperature dependency of fluorescence offset and levels corrections
#'
#' This is an optional function. This function will correct Fm' and F' value including offset and magnitude, due to the effect of temperature on the measuring light. The corresponding Y(II) and ETR were recalculated and all the F', Fm' or Y(II) were removed when they are less than 0. The correction is based on the equations of fluorescence offset correction - Foffset.a X temp_PAM+Foffset.b and fluorescence signal level correction - Flevel.a X temp_PAM+Flevel.b.
#'
#' @usage correctF(PAM.data,Foffset.a,Foffset.b,Flevel.a, Flevel.b, Fmoffset.a, Fmoffset.b, Fmlevel.a, Fmlevel.b)
#' @param PAM.data preprocessed MONI-PAM data
#' @param Foffset.a F' offset correction parameter a
#' @param Foffset.b F' offset correction parameter b
#' @param Flevel.a F' signal correction parameter a
#' @param Flevel.b F' signal correction parameter b
#' @param Fmoffset.a Fm' offset correction parameter a
#' @param Fmoffset.b Fm' offset correction parameter b
#' @param Fmlevel.a Fm' signal correction parameter a
#' @param Fmlevel.b Fm' signal correction parameter b
#'
#' @import dplyr
#'
#' @return This function will data table
#' @export
correctF<-function(PAM.data,
                   Foffset.a = -0.417,
                   Foffset.b = 8.15,
                   Flevel.a = -0.00265,
                   Flevel.b = 1.058,
                   Fmoffset.a = -0.447,
                   Fmoffset.b = 22.62,
                   Fmlevel.a = -0.00224,
                   Fmlevel.b = 1.046){

  PAM.data<-formatPAMdata(PAM.data = PAM.data)

  PAM.data<-
    PAM.data %>%
    # top Foffset.a*temp_PAM+Foffset.b is the offset correction
    # bottom Flevel.a*temp_PAM+Flevel.b is the F magnitude correction
    mutate(F_=(F_-(Foffset.a*temp_PAM+Foffset.b))/
             (Flevel.a*temp_PAM+Flevel.b)) %>%  # correct F'

    mutate(Fm_=(Fm_-(Fmoffset.a*temp_PAM+Fmoffset.b))/
             (Fmlevel.a*temp_PAM+Fmlevel.b)) %>%  # correct Fm'
    mutate(YII=(1-F_/Fm_)) %>% # recalculate YII using corrected F' and Fm'
    mutate(ETR=par_PAM*0.84*0.5*YII) %>%  # recalculate ETR
    subset(F_>0&Fm_>0)# keep the data only when F>0 and Fm>0
  # keep YII only when YII>=0
  PAM.data$YII[PAM.data$YII<=0]<-NA

  return(PAM.data)
}
