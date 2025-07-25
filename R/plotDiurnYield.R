#' Diurnal quantum yield parameters data visualization
#'
#' This function will visualize MONI-PAM diurnal quantum yield parameters data (PhiPSII,PhiNPQT,PhifD, and PhiNPQr) and PAR and temperature in one figure with 10 days as a group plotting for each head and the fieldnote will be marked in the figure if the note file is available. At the same time, this function will also show  the sunlight time (colorful vertical lines) in the plot, sunrise and sunsetStart are 'indianred', solarNoon is 'goldenrod', and dawn and dusk are 'purple'
#'
#' @usage plotDiurnYield(diurnal.para,save.path)
#' @param diurnal.para estimated diurnal quantum yield parameters generated from [diurnalParams] function in this R package
##' @param save.path local folder for saving your plotted figures
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom patchwork plot_layout
#' @importFrom cowplot ggdraw draw_label plot_grid
#' @importFrom ggtext element_markdown
#' @return [plotDiurnYield] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotDiurnYield<-function(diurnal.para,
                         save.path
                         ){

  start.time<-Sys.time()
  diurnal.para<-formatPAMdata(PAM.data = diurnal.para)
  diurnal.para<-diurnal.para %>% mutate(plot.group=as.factor(plot.group))

  plot.title='Diurnal yield parameters'

  lapply(levels(diurnal.para$plot.group),function(i){

    diurnal.para.onegroup<-
      droplevels(diurnal.para[diurnal.para$plot.group==i,])

    if (isTRUE(sum(na.omit(diurnal.para.onegroup$YII))>0)){#when there is data exsit


      plot.onegroup<-
        lapply(levels(diurnal.para.onegroup$head_tree),function(j){


          plot.data<-diurnal.para.onegroup %>% subset(head_tree==j) %>% droplevels()
          plot.data.yield<-
              plot.data %>%
              tidyr::pivot_longer(cols =c("YII","Phi_NPQT","Phi_fD"),
                                  names_to = 'Yield.name',
                                  values_to  ='Yield.value') %>%
              mutate(Yield.name=as.factor(Yield.name)) %>%
              mutate(Yield.value=case_when(
                #Yield.value<0~0,
                #Yield.value>1~1,
                is.na(Yield.value)~0,
                TRUE~Yield.value)) %>%
              mutate(Yield.name=factor(Yield.name,levels=c('YII','Phi_NPQT','Phi_fD')))

          if (isTRUE(sum(na.omit(plot.data$YII))>0&
                     length(na.omit(plot.data$YII))>2)) {#if data exist
            night<-unique(plot.data.yield[,c('date','sunsetStart','sunrise')])
            night<-night[order(night$sunsetStart),]

            if (isTRUE(nrow(night)>1)){
              night$sunriseDay2<-ymd_hms(night$sunrise[2:nrow(night)],NA)
            } else if(isTRUE(nrow(night)==1)){
              night$sunriseDay2<-night$sunrise+3600*24
            }

            night<-merge(plot.data.yield[,c('datetime','date')],night,by='date',all=T)
            night.day1<-plot.data.yield %>% subset(date==min(date,na.rm = T)&datetime<=sunrise)
            night.lastDay<-plot.data.yield %>% subset(date==max(date,na.rm = T)&datetime>=sunsetStart)

            p1<-ggplot(plot.data.yield,aes(x=datetime))+
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

              geom_area(aes(datetime,Yield.value,fill=Yield.name), size=1,alpha=0.5)+
              geom_line(aes(y=(temp_PAM-min(temp_PAM,na.rm = T))/
                              (max(temp_PAM,na.rm = T)-min(temp_PAM,na.rm = T))),
                        color='darkorchid',na.rm = T,size=1)+
              geom_line(aes(y=(par_PAM-min(par_PAM,na.rm = T))/
                              (max(par_PAM,na.rm = T)-min(par_PAM,na.rm = T))),
                        color='yellow3',na.rm = T,size=1)+
              scale_fill_manual(values = c('forestgreen','blue','red'),
                                labels=c(expression(Phi*'P [Y(II)]' ),
                                         expression(Phi*'NPQ'['T']),
                                         expression(Phi*'f,D')))+
              scale_x_datetime(breaks = date_breaks("1 day"),
                               labels = date_format("%b-%d\n%H:%M"),
                               limits = c(range(na.omit(diurnal.para.onegroup$datetime))[1],
                                          range(na.omit(diurnal.para.onegroup$datetime))[2]))+
              scale_y_continuous(
                sec.axis = sec_axis(~.*(max(plot.data.yield$temp_PAM,na.rm = T)-min(plot.data.yield$temp_PAM,na.rm = T))+
                                      min(plot.data.yield$temp_PAM,na.rm = T),
                                    name='Temperature (°C)'),
                name = "Yield" )+
              theme_bw()+
              theme(legend.position =  c(0.035,0.6),
                    legend.title = element_blank(),
                    legend.background = element_blank(),
                    strip.text = element_text(size = 16,color = 'black',face='bold'),
                    strip.placement = 'outside',
                    strip.background = element_blank(),
                    title = element_text(size=12,color='black',face='bold'),
                    axis.text.y.left = element_text(size = 15,color = 'black'),
                    axis.text.y.right = element_text(size = 15,color = 'darkorchid'),
                    axis.text.x =  element_text(size = 15,color = 'black'),
                    axis.title.y.right  = element_markdown(size = 16,color='darkorchid'),
                    axis.title.y.left = element_markdown(size = 16,color = 'black'),
                    axis.title.x =element_blank(),
                    axis.ticks.y.left =element_line(color = 'black'),
                    axis.ticks.y.right =element_line(color = 'darkorchid'),

                    axis.line.y.left = element_line(color = 'black'),
                    axis.line.y.right = element_line(color = 'darkorchid'),

                    plot.margin = unit(c(0,0,0,0),'cm'))

            p2.fc<-function(p2.color){
              ggplot(plot.data.yield,aes(x=datetime))+
                geom_area(aes(datetime,Yield.value),fill='white', size=1,alpha=0.5)+
                geom_line(aes(y=(temp_PAM-min(temp_PAM,na.rm = T))/
                                (max(temp_PAM,na.rm = T)-min(temp_PAM,na.rm = T))),
                          linewidth=0.1,color='white',na.rm = T)+
                geom_line(aes(y=(par_PAM-min(par_PAM,na.rm = T))/
                                (max(par_PAM,na.rm = T)-min(par_PAM,na.rm = T))),
                          linewidth=0.1,color='white',na.rm = T)+
                theme_minimal()+
                theme(legend.position = 'none',
                      axis.text.y.right = element_text(size = 15,color = p2.color),
                      axis.text.y.left = element_blank(),
                      axis.line.y.right = element_line(color = p2.color),
                      axis.text.x =   element_blank(),
                      axis.title.y.right  = element_text(size = 15,color = p2.color),
                      axis.title.y.left = element_blank(),
                      axis.title.x =element_blank(),
                      axis.ticks.y.left = element_blank(),
                      axis.ticks.y.right =element_line(color = p2.color),
                      panel.grid=element_blank(),
                      plot.margin = unit(c(0,0,0,0),'cm'))
            }

            #plot second right-y-axis: PAR
            p2<-p2.fc('yellow3')+
              scale_y_continuous(
                sec.axis = sec_axis(~.*(max(plot.data.yield$par_PAM,na.rm = T)-
                                          min(plot.data.yield$par_PAM,na.rm = T))+
                                      min(plot.data.yield$par_PAM,na.rm = T),
                                    name='PAR'))
            print(paste0('plot.group[',i,']:',j,' is plotted'))
            plot.PAM.figure <- p1 + p2 + plot_layout(nrow =1, widths = c(15, 0.1))
          }
        }
        )

      if (isTRUE(length(plot.onegroup[!sapply(plot.onegroup,is.null)])>0)){

        plot.all<-
        suppressWarnings(plot_grid(plotlist = plot.onegroup[!sapply(plot.onegroup,is.null)],
                    align = 'hv',ncol = 1))

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


