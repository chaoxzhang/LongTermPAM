#' Retrieve FmR and F0R, and visualize Fm vs.Fv/Fm for data before and after filter
#'
#'Reference fluorescence levels (FMR, F0R) are needed to estimate ChlF parameters. These are measured in the absence of NPQ and photoinhibition, with (FV/FM)R typically around 0.82–0.84 in summer. To estimate FMR and F0R, we first identify nightly maximum FV/FM and FM using [FindFVFM] function. If (FV/FM)R values fall within 0.82–0.84, they can be used directly; otherwise, a non-linear regression between FV/FM and FM, implemented in this funciton [ChlFRef], estimates FMR at a user-defined (FV/FM)R (e.g., 0.83). F0R is then calculated as F0R = (1 - (FV/FM)R) * FMR. [ChlFRef] also assesses filtering quality via R² and RRMSE before and after filtering
#'
#' @usage ChlFRef(fvfm.beforeFilter,fvfm.afterFilter,save.file,save.path)
#' @param fvfm.beforeFilter MONI-PAM fvfm data retrieved before data filtering (i.e., flag0.fvfm in Intro_to_LongTermPAM.Rmd) by using [FindFVFM] function in this R package
#' @param fvfm.afterFilter the final MONI-PAM fvfm data retrieved after data filtering (i.e., flag6.fvfm in Intro_to_LongTermPAM.Rmd) by using [FindFVFM] function in this R package
#' @param FvFmR the maximum Fv/Fm value when there is no NPQ or NPQ is close to 0. For example, for Scots pine needles, Fv/FmR can be around 0.83.
#' @param save.file TRUE or FALSE. If TRUE, plotted figures and output data will be saved to local folder via save.path argument
#' @param save.path local folder for saving the plotted figures and output data generated from this function
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @import dplyr
#' @importFrom plyr ldply
#' @import ggplot2
#' @import scales
#' @importFrom cowplot plot_grid
#' @return This function will return (1) a data frame including estimated FmR, F0R and the summary (R2, bias,RMSE,and RRMSE) of non-linear regression model simulation between Fm and Fv/Fm data from entire observation period, and (2) a corresponding figure.
#' @export
ChlFRef<-function(fvfm.beforeFilter,
                  fvfm.afterFilter,
                  FvFmR,
                  save.file,
                  save.path){

  fvfm.afterFilter<-
    fvfm.afterFilter %>%
    mutate(date=as.Date(date),
           head_tree=as.factor(head_tree)) %>%
    mutate(season=paste0(range(lubridate::year(date))[1],'_',
                         range(lubridate::year(date))[2]),
           var='Before filter')

  fvfm.beforeFilter<-
    fvfm.beforeFilter %>%
    mutate(date=as.Date(date),
           head_tree=as.factor(head_tree)) %>%
    mutate(season=paste0(range(lubridate::year(date))[1],'_',
                         range(lubridate::year(date))[2]),
           var='After filter')

  FmR.fc<-function(data){

    resul<-
      lapply(levels(data$head_tree),function(i){

        onetree<-data %>%
          subset(head_tree==i,
                 select=c('var','date','season','head_tree','head',
                          'tree_num','Fm','FvFm')) %>%
          na.omit()


        # fit the Fm and FvFm using non linear regression model(nls), if nls does
        # not work, then using linear regression model (lm)
        m<-tryCatch({
          nls(Fm ~ m * exp(n*FvFm),data=onetree,
              start = list(m=1,n=1),
              algorithm = "port",
              control = nls.control(maxiter = 4000))
          }, error=function(e) {
           lm(Fm~FvFm,data = onetree)
         })

        #get coefficients
        coef.m<-summary(m)$coef[1,1] # m in nls model;  intercept in lm model
        coef.n<-summary(m)$coef[2,1] # n in nls model; slope in lm model,

        nls.fc<-tryCatch({
          nls(Fm ~ m * exp(n*FvFm),data=onetree,
              start = list(m=1,n=1),
              algorithm = "port",
              control = nls.control(maxiter = 4000))
        }, error=function(e) {
          return('An error occured in nls fitting,so using lm fitting')
        })


        if (isTRUE('An error occured in nls fitting,so using lm fitting'%in%nls.fc)){
          FmR=coef.n * FvFmR+coef.m

        } else {
          FmR=coef.m * exp(coef.n*FvFmR)

        }


        # prepare for calculating R2, bias, RMSE, and RRMSE
        y_pred <- predict(m,newdata = onetree)
        residuals <- residuals(m)
        SS_tot <- sum((onetree$Fm- mean(onetree$Fm))^2)
        SS_res <- sum(residuals^2)
        RMSE=sqrt(mean((onetree$Fm - y_pred)^2, na.rm = TRUE))

        FR<-
          data.frame(
            onetree %>% select('var','season','head_tree','head','tree_num') %>% unique(),
            FvFmR=FvFmR,
            FmR=FmR,
            F0R=(1-FvFmR)*FmR,# F0R=(1-FvFmR)*FmR
            R2= 1-(SS_res/SS_tot),   #calculate R2
            N=nrow(onetree), # number of points used in the regression model
            RMSE=RMSE,# RMSE
            RRMSE=RMSE / diff(range(onetree$Fm , na.rm = TRUE)) # Calculate RRMSE
            ) %>%
          mutate(R2=round(R2,digits = 2)) %>%
          mutate(RMSE=round(RMSE,digits = 2)) %>%
          mutate(RRMSE=round(RRMSE,digits = 2)) %>%
          mutate(FmR=round(FmR,digits = 0)) %>%
          mutate(F0R=round(F0R,digits = 0))

        custom_labels <- function(x) {
          ifelse(x == FvFmR, paste0("<span style='color:red;'>", x, "</span>"), x)
        }

        p<-
          ggplot(onetree,aes(FvFm,Fm))+
          geom_point(aes(color=lubridate::month(date)),size=1,na.rm = T)+
          geom_line(aes(FvFm,predict(m,newdata = data.frame(FvFm))),
                    color='black',linewidth=1,na.rm = T)+
          geom_vline(xintercept = FvFmR,linetype=2)+
          geom_hline(yintercept = FR$FmR,linetype=2)+
          annotate('point',x=FvFmR,y=FR$FmR,color='red',size=3,na.rm = T,alpha=0.7)+
          scale_x_continuous(breaks = c(seq(0,0.6,0.1),FvFmR),
                             name = expression(paste('F'['V'],'/','F'['M'])),
                             labels = custom_labels)+
          scale_color_gradientn(colours = rainbow(6),name = 'month')+
          ylab(expression(paste('F'['M'])))+
          ggtitle(i)+
          geom_text(data = FR, color='black',hjust=3,vjust=1,size=4,
                    mapping=aes(x=Inf,y=Inf,label=paste0('R2','=',R2)))+
          geom_text(data = FR, color='black',hjust=2.3,vjust=3,size=4,
                    mapping=aes(x=Inf,y=Inf,label=paste0('N=',N)))+
          geom_text(data = FR, color='black',hjust=2,vjust=5,size=4,
                    mapping=aes(x=Inf,y=Inf,label=paste0('RRMSE=',RRMSE)))+
          geom_text(data = FR, color='red',hjust=2.3,vjust=7,size=4,
                    mapping=aes(x=Inf,y=Inf,label=paste0('FmR','=',FmR)))+
          geom_text(data = FR, color='red',hjust=2.6,vjust=9,size=4,
                    mapping=aes(x=Inf,y=Inf,label=paste0('F0R','=',F0R)))+
          theme_bw()+
          theme(axis.text.y = element_text(color='black',size=11),
                axis.text.x = element_markdown(color='black',size=11),
                axis.title = element_text(size=15),
                plot.title= element_text(hjust=0.5),
                legend.key.width = unit(0.2,'cm'))

        return(list(FR,p))
      })
    }

  # calculate fluorescence reference and plot the results for both data before and after filtering
  FmR.res.raw<-FmR.fc(fvfm.beforeFilter);FmR.res.filter<-FmR.fc(fvfm.afterFilter)
  # combine results data into one data file
  FmR.res<-rbind(ldply(1:length(FmR.res.raw),function(i){FmR.res.raw[[i]][[1]]}),
                 ldply(1:length(FmR.res.filter),function(i){FmR.res.filter[[i]][[1]]}))
  # retrieve plotted result figure list for both data before and after filtering
  fig.raw.list<-lapply(1:length(FmR.res.raw),function(i){FmR.res.raw[[i]][[2]]})
  fig.filter.list<-lapply(1:length(FmR.res.filter),function(i){FmR.res.filter[[i]][[2]]})

  # combine figure list into one for data before filtering
  fig.raw<-do.call(plot_grid, c(fig.raw.list, nrow = 1,align='hv',
                                labels='Before filter',label_size=16,label_x=-0.1,label_y=1.02))
  # combine figure list into one for data after filtering
  fig.filter<-do.call(plot_grid, c(fig.filter.list, nrow = 1,align='hv',
                                   labels='After filter',label_size=16,label_x=-0.05,label_y=1.02))
  # combined figures from data before and after filtering into one
  fig.all<-plot_grid(fig.raw,fig.filter,align = 'hv',ncol = 1)
  print(fig.all)

  if (save.file==T){

    ggsave(fig.all,compression='lzw',dpi=200,
            height=6,width = 4*length(levels(fvfm.afterFilter$head_tree)),
            filename = paste0(save.path,'/',unique(FmR.res$season),
                              '_','FmR_F0R','.tiff'))

    write.table(FmR.res,row.names = F,sep=';',
                file=paste0(save.path,'/',unique(FmR.res$season),
                            '_','FmR_F0R','.csv'))
  }
  return(FmR.res)
}
