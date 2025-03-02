
scaleQuench<-function(diurnal.para){

  diurnal.para<-formatPAMdata(PAM.data = diurnal.para)
  data<-diurnal.para %>%
    dplyr::select(plot.group,head,tree_num,head_tree,
                  PQ,NPQT,NPQr,par_PAM,temp_PAM)

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
        .col=c('PQ','NPQT','NPQr','par_PAM','temp_PAM'),
        .fns = list(max = safe_max, min = safe_min),
        .names = "{.fn}.{.col}"
      ),
      .groups = 'drop'
    )

  maxmin.res<-
    maxmin.res %>%
    mutate(scale.NPQ.a=(max.PQ-min.PQ)/(max.NPQT-min.NPQT)) %>%
    mutate(scale.NPQ.b=min.PQ-scale.NPQ.a*min.NPQT) %>%
    mutate(scale.temp.a=(max.PQ-min.PQ)/(max.temp_PAM-min.temp_PAM)) %>%
    mutate(scale.temp.b=min.PQ-scale.temp.a*min.temp_PAM) %>%
    mutate(scale.par.a=(max.PQ-min.PQ)/(max.par_PAM-min.par_PAM)) %>%
    mutate(scale.par.b=min.PQ-scale.par.a*min.par_PAM)


  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res[sapply(maxmin.res, is.nan)]<-NA
  maxmin.res[,c(15,17,19)][sapply(maxmin.res[,c(15,17,19)],is.na)]<-1
  maxmin.res[,c(16,18,20)][sapply(maxmin.res[,c(16,18,20)],is.na)]<-0
  return(maxmin.res)
}


#' Diurnal quenching parameters data visualization
#'
#' This function visualizes MONI-PAM diurnal quenching parameters (PQ,qLT,NPQT, and NPQr), and PAR and temperature in one figure with 10 days as a group plotting for each head. At the same time, this function will also show  the sunlight time (colorful vertical lines) in the plot.
#'
#' @usage plotDiurnQuench(diurnal.para,save.path)
#' @param diurnal.para estimate diurnal parameters generated from [diurnalParams] function in this R package
#' @param save.path local folder for saving your plotted figures
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom patchwork plot_layout
#' @importFrom cowplot ggdraw draw_label plot_grid
#' @importFrom ggtext element_markdown
#'
#' @return [plotDiurnQuench] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotDiurnQuench<-function(diurnal.para,
                          save.path
                                ){

  start.time<-Sys.time()
  #If a warning like the following one is printed, please just omit it.
  #This warning is not mean error or mistake.
  #[warning:No non-missing values found in at least one group.
  #Returning '-Inf' for such groups to be consistent
  #with baseNo non-missing values found in at least one group]
  diurnal.para<-formatPAMdata(PAM.data = diurnal.para)
  diurnal.para<-diurnal.para %>% mutate(plot.group=as.factor(plot.group))

  extreme.data<-scaleQuench(diurnal.para)
  plot.title='Diurnal quenching parameters'

    lapply(levels(diurnal.para$plot.group),function(i){

    diurnal.para.onegroup<-
      droplevels(diurnal.para[diurnal.para$plot.group==i,])

    if (isTRUE(sum(na.omit(diurnal.para.onegroup$YII))>0)){#when there is data exsit

      scaledata.onegroup<-extreme.data %>% subset(plot.group==i) %>% droplevels()

      plot.onegroup<-
        lapply(levels(diurnal.para.onegroup$head_tree),function(j){

          scale.data<-scaledata.onegroup %>% subset(head_tree==j) %>% droplevels()

          NPQtoPQ<-function(x){scale.data$scale.NPQ.a*x+scale.data$scale.NPQ.b}
          TtoPQ<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
          PARtoPQ<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}

          plot.data<-diurnal.para.onegroup %>% subset(head_tree==j) %>% droplevels()

          if (isTRUE(sum(na.omit(plot.data$YII))>0&
                     length(na.omit(plot.data$YII))>2)) {#if data exist
            night<-unique(plot.data[,c('date','sunsetStart','sunrise')])
            night<-night[order(night$sunsetStart),]

            if (isTRUE(nrow(night)>1)){
              night$sunriseDay2<-ymd_hms(night$sunrise[2:nrow(night)],NA)
            } else if(isTRUE(nrow(night)==1)){
              night$sunriseDay2<-night$sunrise+3600*24
            }

            night<-merge(plot.data[,c('datetime','date')],night,by='date',all=T)

            night.day1<-plot.data %>% subset(date==min(date,na.rm = T)&datetime<=sunrise)
            night.lastDay<-plot.data %>% subset(date==max(date,na.rm = T)&datetime>=sunsetStart)

            p1<-ggplot(plot.data,aes(x=datetime))+
              facet_grid(head_tree~.,switch ='y' )+
              geom_rect(data=night,fill='lightskyblue1',na.rm = T,alpha=0.008,
                        aes(xmin=sunsetStart,xmax=sunriseDay2,
                            ymin=-Inf,ymax=Inf))+

              geom_rect(data=night.day1,fill='lightskyblue1',na.rm = T,alpha=0.008,
                        aes(xmin=min(datetime,na.rm = T),xmax=max(datetime,na.rm = T),
                            ymin=-Inf,ymax=Inf))+
              geom_rect(data=night.lastDay,fill='lightskyblue1',na.rm = T,alpha=0.008,
                        aes(xmin=min(datetime,na.rm = T),xmax=max(datetime,na.rm = T),
                            ymin=-Inf,ymax=Inf))+

              geom_vline(aes(xintercept=sunrise),na.rm = T,
                         color='indianred1',linetype=2)+
              geom_vline(aes(xintercept=solarNoon),na.rm = T,
                         color='goldenrod1',linetype=2)+
              geom_vline(aes(xintercept=sunsetStart),na.rm = T,
                         color='indianred1',linetype=2)+
              geom_vline(aes(xintercept=dusk),na.rm = T,
                         color='purple',linetype=2)+
              geom_vline(aes(xintercept=dawn),na.rm = T,
                         color='purple',linetype=2)+
              geom_line(aes(y=TtoPQ(temp_PAM)),color='darkorchid',na.rm = T)+
              geom_line(aes(y=PARtoPQ(par_PAM)),color='yellow3',na.rm = T)+
              geom_line(aes(y=NPQtoPQ(NPQr)),color='black',na.rm = T)+
              geom_point(aes(y=NPQtoPQ(NPQr)),size=0.5,color='black',na.rm = T)+
              geom_line(aes(y=NPQtoPQ(NPQT)),color='darkred',na.rm = T)+
              geom_point(aes(y=NPQtoPQ(NPQT)),color='darkred',size=0.5,na.rm = T)+

              geom_line(aes(y=qLT*2),color='blue',na.rm = T,alpha=0.3)+
              geom_point(aes(y=qLT*2),size=1,color='blue',na.rm = T,alpha=0.3)+

              geom_line(aes(y=PQ),color='forestgreen',na.rm = T)+
              geom_point(aes(y=PQ),size=0.5,color='forestgreen',na.rm = T)+

              scale_x_datetime(breaks = date_breaks("1 day"),
                               labels = date_format("%b-%d\n%H:%M"),
                               limits = c(range(na.omit(diurnal.para.onegroup$datetime))[1],
                                          range(na.omit(diurnal.para.onegroup$datetime))[2]))+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.NPQ.b)/scale.data$scale.NPQ.a,
                                    name=" <span style='color: darkred;'>NPQ or <span style='color:black;'>NPQr"),
                name = " <span style='color:forestgreen;'>PQ or <span style='color:blue;'>qLT X 2")+
              theme_bw()+
              theme(legend.position = 'none',
                    strip.text = element_text(size = 16,color = 'black',face='bold'),
                    strip.placement = 'outside',
                    strip.background = element_blank(),
                    title = element_text(size=12,color='black',face='bold'),
                    axis.text.y.left = element_text(size = 15,color = 'black'),
                    axis.text.y.right = element_text(size = 15,color = 'black'),
                    axis.text.x =  element_text(size = 15,color = 'black'),
                    axis.title.y.right  = element_markdown(size = 16),
                    axis.title.y.left = element_markdown(size = 16,color = 'forestgreen'),
                    axis.title.x =element_blank(),
                    axis.ticks.y.left =element_line(color = 'forestgreen'),
                    axis.line.y.left = element_line(color = 'forestgreen'),
                    plot.margin = unit(c(0,0,0,0),'cm'))



            p23.fc<-function(p23.color){
              ggplot(plot.data,aes(x=datetime))+
                geom_line(aes(y=TtoPQ(temp_PAM)),linewidth=0.1,color='white',na.rm = T)+
                geom_line(aes(y=PARtoPQ(par_PAM)),linewidth=0.1,color='white',na.rm = T)+
                geom_line(aes(y=PQ),color='white',na.rm = T)+
                geom_point(aes(y=PQ),size=1,color='white',na.rm = T)+
                geom_line(aes(y=qLT*2),color='white',na.rm = T)+
                geom_point(aes(y=qLT*2),size=1,color='white',na.rm = T)+

                geom_line(aes(y=NPQtoPQ(NPQT)),color='white',na.rm = T)+
                geom_point(aes(y=NPQtoPQ(NPQT)),size=1,color='white',na.rm = T)+
                geom_line(aes(y=NPQtoPQ(NPQr)),color='white',na.rm = T)+
                geom_point(aes(y=NPQtoPQ(NPQr)),color='white',size=1,na.rm = T)+
                theme_minimal()+
                theme(legend.position = 'none',
                      axis.text.y.right = element_text(size = 15,color = p23.color),
                      axis.text.y.left = element_blank(),
                      axis.line.y.right = element_line(color = p23.color),
                      axis.text.x =   element_blank(),
                      axis.title.y.right  = element_text(size = 15,color = p23.color),
                      axis.title.y.left = element_blank(),
                      axis.title.x =element_blank(),
                      axis.ticks.y.left = element_blank(),
                      axis.ticks.y.right =element_line(color = p23.color),
                      panel.grid=element_blank(),
                      plot.margin = unit(c(0,0,0,0),'cm'))
            }
            #plot second left-y-axis: Temperature
            p2<-p23.fc('darkorchid')+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.temp.b)/scale.data$scale.temp.a,
                                    name=expression(paste('Temp ('^'o','C)'))))
            #plot third left-y-axis: PAR
            p3<-p23.fc('yellow3')+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.par.b)/scale.data$scale.par.a,
                                    name='PAR'))
            print(paste0('plot.group[',i,']:',j,' is plotted'))
            plot.PAM.figure <- p1 + p2 + p3 +
              plot_layout(nrow =1, widths = c(15, 0.1, 0.1))
          }
        }
        )

      if (isTRUE(length(plot.onegroup[!sapply(plot.onegroup,is.null)])>0)){

        plot.all<-
          plot_grid(plotlist = plot.onegroup[!sapply(plot.onegroup,is.null)],
                    align = 'hv',ncol = 1)

        title <- ggdraw() +
          draw_label(paste0(plot.title,
                            ' from ',range(na.omit(diurnal.para.onegroup$date))[1],
                            ' to ', range(na.omit(diurnal.para.onegroup$date))[2]),
                     x = 0.05,hjust = 0,fontface='bold',size=20)+
          theme(plot.margin = margin(0,0,0,0))

        plot.all.title<-
          plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))

        daylength.x<-as.numeric(diff(range(na.omit(diurnal.para.onegroup$date))))
        headlength.y<-length(plot.onegroup[!sapply(plot.onegroup,is.null)])
        ggsave(plot.all.title,
               filename = paste0(save.path,'/',plot.title,
                                 ' from ',range(na.omit(diurnal.para.onegroup$date))[1],
                                 ' to ',range(na.omit(diurnal.para.onegroup$date))[2],'.tiff'),
               compression='lzw',dpi=100,width = 1.5*daylength.x+6,
               height = 2.3*headlength.y+0.3)

      }
    }
  })
  print(Sys.time()-start.time)

}
