#' Retrieve FmR and FoR, and visualize Fm vs.Fv/Fm from raw and filtered data
#'
#' This function will estimate FmR and FoR, and visualize the scatter plot between Fm and Fv/Fm from both raw and filtered data. The purpose of this visualization is to validate MONIPAM data filter. To calculate other chlorophyll fluorescence parameters, we firstly need to know Fm reference (FmR) and Fo reference (FoR) when there is no NPQ or NPQ is near 0. In this case, Fv/Fm will be maximum value for whole observation period. For example, Scots pine needles usually have maximum Fv/Fm (FvFmR) around 0.84. Additionaly, Fv/Fm and Fm usually have a clear similar varying pattern. Therefore, We built a non-linear regression (exponential) model between Fm and Fv/Fm: Fm = a X exp (b x Fv/Fm) to estimate FmR based on FvFmR. You can define FvFmR value based on the species you measured.
#'
#' @usage ChlFRef(raw.fvfm,filter.fvfm,save.file,save.path)
#' @param raw.fvfm MONI-PAM fvfm data retrieved from original data (i.e., flag0.fvfm in Intro_to_LongTermPAM.Rmd) by using [FindFvFm] function in this R package
#' @param filter.fvfm the final MONI-PAM fvfm data retrieved from cleaning step 6 (i.e., flag6.fvfm in Intro_to_LongTermPAM.Rmd) by using [FindFvFm] function in this R package
#' @param FvFmR the maximum Fv/Fm value when there is no NPQ or NPQ is close to 0. For example, for Scots pine needles, Fv/FmR can be around 0.84.
#' @param save.file TRUE or FALSE. If TRUE, plotted figures and output data will be saved to local folder via save.path argument
#' @param save.path local folder for saving the plotted figures and output data generated from this function
#' @importFrom lubridate year month
#' @import data.table
#' @import dplyr
#' @importFrom plyr ldply
#' @import ggplot2
#' @import scales
#' @importFrom cowplot plot_grid
#' @return This function will return (1) a data frame including estimated FmR, FoR and the summary (R2, bias,RMSE,and RRMSE) of non-linear regression model simulation between Fm and Fv/Fm data from entire observation period, and (2) a corresponding figure.
#' @export
ChlFRef<-function(raw.fvfm,
                  filter.fvfm,
                  FvFmR,
                  save.file,
                  save.path){

  filter.fvfm<-
    filter.fvfm %>%
    mutate(date=as.Date(date),
           head_tree=as.factor(head_tree)) %>%
    mutate(season=paste0(range(lubridate::year(date))[1],'_',
                         range(lubridate::year(date))[2]),
           var='Filtered data')

  raw.fvfm<-
    raw.fvfm %>%
    mutate(date=as.Date(date),
           head_tree=as.factor(head_tree)) %>%
    mutate(season=paste0(range(lubridate::year(date))[1],'_',
                         range(lubridate::year(date))[2]),
           var='Raw data')

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
            FoR=(1-FvFmR)*FmR,# FoR=(1-FvFmR)*FmR
            R2= 1-(SS_res/SS_tot),   #calculate R2
            N=nrow(onetree), # number of points used in the regression model
            RMSE=RMSE,# RMSE
            RRMSE=RMSE / diff(range(onetree$Fm , na.rm = TRUE)) # Calculate RRMSE
            ) %>%
          mutate(R2=round(R2,digits = 2)) %>%
          mutate(RMSE=round(RMSE,digits = 2)) %>%
          mutate(RRMSE=round(RRMSE,digits = 2)) %>%
          mutate(FmR=round(FmR,digits = 0)) %>%
          mutate(FoR=round(FoR,digits = 0))

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
                    mapping=aes(x=Inf,y=Inf,label=paste0('FoR','=',FoR)))+
          theme_bw()+
          theme(axis.text.y = element_text(color='black',size=11),
                axis.text.x = element_markdown(color='black',size=11),
                axis.title = element_text(size=15),
                plot.title= element_text(hjust=0.5),
                legend.key.width = unit(0.2,'cm'))

        return(list(FR,p))
      })
    }

  # calculate fluorescence reference and plot the results for both raw  and filtered data
  FmR.res.raw<-FmR.fc(raw.fvfm);FmR.res.filter<-FmR.fc(filter.fvfm)
  # combine results data into one data file
  FmR.res<-rbind(ldply(1:length(FmR.res.raw),function(i){FmR.res.raw[[i]][[1]]}),
                 ldply(1:length(FmR.res.filter),function(i){FmR.res.filter[[i]][[1]]}))
  # retrieve plotted result figure list for both raw and filtered data
  fig.raw.list<-lapply(1:length(FmR.res.raw),function(i){FmR.res.raw[[i]][[2]]})
  fig.filter.list<-lapply(1:length(FmR.res.filter),function(i){FmR.res.filter[[i]][[2]]})

  # combine figure list into one for raw data
  fig.raw<-do.call(plot_grid, c(fig.raw.list, nrow = 1,align='hv',
                                labels='raw data',label_size=16,label_x=-0.1,label_y=1.02))
  # combine figure list into one for filtered data
  fig.filter<-do.call(plot_grid, c(fig.filter.list, nrow = 1,align='hv',
                                   labels='filtered data',label_size=16,label_x=-0.15,label_y=1.02))
  # combined figures from raw data and filtered data into one
  fig.all<-plot_grid(fig.raw,fig.filter,align = 'hv',ncol = 1)
  print(fig.all)

  if (save.file==T){

    ggsave(fig.all,compression='lzw',dpi=200,
            height=6,width = 4*length(levels(filter.fvfm$head_tree)),
            filename = paste0(save.path,'/',unique(FmR.res$season),
                              '_','FmR_FoR','.tiff'))

    write.table(FmR.res,row.names = F,sep=';',
                file=paste0(save.path,'/',unique(FmR.res$season),
                            '_','FmR_FoR','.csv'))
  }
  return(FmR.res)
}
