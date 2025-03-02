#' MONI-PAM data visualization
#'
#' This function visualizes MONI-PAM data including F',FM',Y(II),PAR, and temperature in a single figure, grouping data into 10-day intervals for each head. It also displays sunlight time using colorful vertical lines: sunrise and sunsetStart in 'indianred', solarNoon in 'goldenrod', and dawn and dusk in 'purple'. These sunlight times will be used in the data filtering process.
#'
#' @usage plotPAM(PAM.data,plot.title,save.path)
#' @param PAM.data a combined and organized MONI-PAM dataset generated from [readPAM] function or after applying [correctF] function, or a MONI-PAM data after data filtering which is generated using filtering functions in this package, such as [filter1.NA], [filter2.night] and so on.
#' @param plot.title any text to describe clearly about your figure content
#' @param save.path local folder for saving the plotted figures generated from this function
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom patchwork plot_layout
#' @importFrom cowplot ggdraw draw_label plot_grid
#' @importFrom ggtext element_markdown
#' @return [plotPAM] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotPAM<-function(PAM.data,
                  plot.title,
                  save.path
                  ){

  start.time<-Sys.time()
  #If a warning like the following one is printed, please just omit it.
  #This warning is not mean error or mistake.
  #[warning:No non-missing values found in at least one group.
  #Returning '-Inf' for such groups to be consistent
  #with baseNo non-missing values found in at least one group]
  PAM.data<-formatPAMdata(PAM.data=PAM.data)
  extreme.data<-scalePAMdata(PAM.data)

  lapply(levels(PAM.data$plot.group),function(i){

    PAM.data.onegroup<-
      droplevels(PAM.data[PAM.data$plot.group==i,])

    if (isTRUE(sum(na.omit(PAM.data.onegroup$YII))>0)){#when there is data exsit

      scaledata.onegroup<-
        droplevels(extreme.data[extreme.data$plot.group==i,])

      plot.onegroup<-
        lapply(levels(PAM.data.onegroup$head_tree),function(j){

          scale.data<-
            droplevels(scaledata.onegroup[scaledata.onegroup$head_tree==j,])

          FtoYII<-function(x){scale.data$scale.Fm.a*x+scale.data$scale.Fm.b}
          TtoYII<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
          PARtoYII<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}

          plot.data<-
            droplevels(PAM.data.onegroup[PAM.data.onegroup$head_tree==j,])


          if (isTRUE(sum(na.omit(plot.data$YII))>0&
                     length(na.omit(plot.data$YII))>2)) {#if data exist
              night<-unique(plot.data[,c('date','sunsetStart','sunrise')])
              night<-night[order(night$sunsetStart),]

              if (isTRUE(nrow(night)>1)){
                night$sunriseDay2<-ymd_hms(night$sunrise[2:nrow(night)],NA)
                night$midnight<-ymd_hms(paste(night$date+1,'00:00:00'),truncated = 3)

                } else if(isTRUE(nrow(night)==1)){
                  night$sunriseDay2<-night$sunrise+3600*24
                  night$midnight<-ymd_hms(paste(night$date+1,'00:00:00'),truncated = 3)

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
                geom_line(aes(y=TtoYII(temp_PAM)),color='darkorchid',na.rm = T)+
                geom_line(aes(y=PARtoYII(par_PAM)),color='yellow3',na.rm = T)+
                geom_line(aes(y=FtoYII(F_)),color='grey30',na.rm = T)+
                geom_point(aes(y=FtoYII(F_)),size=0.5,color='grey30',na.rm = T)+
                geom_line(aes(y=FtoYII(Fm_)),color='black',na.rm = T)+
                geom_point(aes(y=FtoYII(Fm_)),color='black',size=0.5,na.rm = T)+
                geom_line(aes(y=YII),color='forestgreen',na.rm = T)+
                geom_point(aes(y=YII),size=0.5,color='forestgreen',na.rm = T)+
                scale_x_datetime(breaks = date_breaks("1 day"),
                                 labels = date_format("%b-%d\n%H:%M"),
                                 limits = c(range(na.omit(PAM.data.onegroup$datetime))[1],
                                            range(na.omit(PAM.data.onegroup$datetime))[2]))+
                scale_y_continuous(
                  sec.axis = sec_axis(~(.-scale.data$scale.Fm.b)/scale.data$scale.Fm.a,
                                      name="Fm' or <b style='color:#808080'>F'</b>"),
                  name = "<b style='color:forestgreen'>&#x03A6;P [Y(II)]</b>")+
                theme_bw()+
                theme(legend.position = 'none',
                      strip.text = element_text(size = 14,color = 'black',face='bold'),
                      strip.placement = 'outside',
                      strip.background = element_blank(),
                      title = element_text(size=12,color='black',face='bold'),
                      axis.text.y.left = element_text(size = 15,color = 'forestgreen'),
                      axis.text.y.right = element_text(size = 15,color = 'black'),
                      axis.text.x =  element_text(size = 15,color = 'black'),
                      axis.title.y.right  = element_markdown(size = 16),
                      axis.title.y.left = element_markdown(size = 16,color = 'black'),
                      axis.title.x =element_blank(),
                      axis.ticks.y.left =element_line(color = 'forestgreen'),
                      axis.line.y.left = element_line(color = 'forestgreen'),
                      plot.margin = unit(c(0,0,0,0),'cm'))



              p23.fc<-function(p23.color){
                ggplot(plot.data,aes(x=datetime))+
                  geom_line(aes(y=TtoYII(temp_PAM)),linewidth=0.1,color='white',na.rm = T)+
                  geom_line(aes(y=PARtoYII(par_PAM)),linewidth=0.1,color='white',na.rm = T)+
                  geom_line(aes(y=YII),color='white',na.rm = T)+
                  geom_point(aes(y=YII),size=1,color='white',na.rm = T)+
                  geom_line(aes(y=FtoYII(F_)),color='white',na.rm = T)+
                  geom_point(aes(y=FtoYII(F_)),size=1,color='white',na.rm = T)+
                  geom_line(aes(y=FtoYII(Fm_)),color='white',na.rm = T)+
                  geom_point(aes(y=FtoYII(Fm_)),color='white',size=1,na.rm = T)+
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
              #plot second right-y-axis: Temperature
              p2<-p23.fc('darkorchid')+
                scale_y_continuous(
                  sec.axis = sec_axis(~(.-scale.data$scale.temp.b)/scale.data$scale.temp.a,
                                      name=expression(paste('Temperature ('^'o','C)'))))
              #plot third right-y-axis: PAR
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
                            ' from ',range(na.omit(PAM.data.onegroup$date))[1],
                            ' to ', range(na.omit(PAM.data.onegroup$date))[2]),
                     x = 0.05,hjust = 0,fontface='bold',size=20)+
          theme(plot.margin = margin(0,0,0,0))

        plot.all.title<-
          plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))

        daylength.x<-as.numeric(diff(range(na.omit(PAM.data.onegroup$date))))
        headlength.y<-length(plot.onegroup[!sapply(plot.onegroup,is.null)])
        ggsave(plot.all.title,
               filename = paste0(save.path,'/',plot.title,
                                 ' from ',range(na.omit(PAM.data.onegroup$date))[1],
                                 ' to ',range(na.omit(PAM.data.onegroup$date))[2],'.tiff'),
               compression='lzw',dpi=100,width = 1.5*daylength.x+6,
               height = 2.3*headlength.y+0.3)

      }
    }
  })
  print(Sys.time()-start.time)

}
