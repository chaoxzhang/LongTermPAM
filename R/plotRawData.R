#' MONI-PAM data visualization with field-note and sunlight time marked in the figure
#'
#' This function will visualize MONI-PAM data (F',FM',YII,PAR, and temperature in one figure) with 10 days as a group plotting for each head and the dairy note will be marked in the figure if the note file is available. At the same time, this function will also show  the sunlight time (colorful vertical lines) in the plot, sunrise and sunsetStart are 'indianred', solarNoon is 'goldenrod', and dawn and dusk are 'purple'
#'
#' @usage plotRawData(PAM.data,fieldnote,fieldnote.data,plot.title,save.path)
#' @param PAM.data a combined organized MONI-PAM data which is generated from [readPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.NA], [filter2.night] and so on.
#' @param fieldnote TRUE or FALSE. If fieldnote = TRUE, fieldnote data should be assigned.
#' @param fieldnote.data a fieldnote file which can be such as .csv or .dat file but with a specific data format. You can check the example data from this package using data("fieldnote_2014_2015"). In this dataset, head and tree_num should be same with your MONI-PAM data, text in 'remark.plot' column will be shown in the plotted figure to show what has happened for that 'head'/ 'tree_num'/'head_tree' on which 'datetime' so it should be very short and clean. 'remark' column can can contain full description for remark.plot. 'manager' column shows who found that issue or conducted certain activity for that sensor (e.g., calibration)
#' @param plot.title any text to describe clearly about your figure content
#' @param save.path local folder for saving your plotted figures
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom patchwork plot_layout
#' @importFrom cowplot ggdraw draw_label plot_grid
#'
#' @return [plotRawData] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotRawData<-function(PAM.data,
                      fieldnote,
                      fieldnote.data,
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
  if (isTRUE(fieldnote==TRUE)){
    fieldnote.data=fieldnote.data
    fieldnote.data$datetime<-ymd_hms(fieldnote.data$datetime,truncated = 3)
  }

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

          if (isTRUE(fieldnote==TRUE)){

            fieldnote.group<-
              droplevels(setDT(fieldnote.data)[fieldnote.data$head_tree==j&
                                             fieldnote.data$plot.group==i,
                                           c('datetime','head_tree',
                                             'plot.group','remark.plot')])

          if (isTRUE(length(plot.data$YII[!is.na(plot.data$YII)])<=2&#if YII does not exist
              length(plot.data$par_PAM[!is.na(plot.data$par_PAM)])<=2&#if PAR and T also do not exist
              length(fieldnote.group$remark.plot[!is.na(fieldnote.group$remark.plot)])>0#but there is a fieldnote,
              #then only plot text of remark.plot column in fieldnote data
              )) {
            print(paste0('plot.group[',i,']:',j))
            plot.PAM.figure<-
              ggplot(fieldnote.group,aes(x=datetime))+
              geom_point(aes(y=0.55), color='red',size=4,na.rm = T)+
              geom_text(aes(y=0.6,label=remark.plot),color='red',size=5,na.rm = T)+
              scale_x_datetime(breaks = date_breaks('1 day'),
                               labels = date_format("%b-%d\n%H:%M"),
                               limits = c(range(na.omit(plot.data$datetime))[1],
                                          range(na.omit(plot.data$datetime))[2]))+
              ylim(0.4,0.7)
            } else if (isTRUE(length(plot.data$YII[!is.na(plot.data$YII)])<=2&#if YII does not exist
                     length(plot.data$par_PAM[!is.na(plot.data$par_PAM)])>2&#if PAR and T exist
                     length(fieldnote.group$remark.plot[!is.na(fieldnote.group$remark.plot)])>0#but there is a fieldnote,
                     #then plot PAR and Temperature with remark.plot
                     )) {
              print(paste0('plot.group[',i,']:',j))
              point.posi<-mean(plot.data$temp_PAM,na.rm = T)
              scale.parA<-
                (max(plot.data$temp_PAM,na.rm=T)-min(plot.data$temp_PAM,na.rm=T))/
                (max(plot.data$par_PAM,na.rm=T)-min(plot.data$par_PAM,na.rm=T))
              scale.parB<-
                max(plot.data$temp_PAM,na.rm=T)-
                scale.parA*max(plot.data$par_PAM,na.rm=T)

              p1<-
                ggplot(plot.data,aes(x=datetime))+
                geom_point(data=fieldnote.group,aes(y=point.posi), color='red',size=2,na.rm = T)+
                geom_text(data=fieldnote.group,aes(y=point.posi+2,label=remark.plot),
                          color='red',size=5,na.rm = T)+
                geom_line(aes(y=temp_PAM),color='darkorchid',na.rm = T)+
                geom_line(aes(y=scale.parA*par_PAM+scale.parB),
                          color='yellow3',na.rm = T)+
                scale_x_datetime(breaks = date_breaks('1 day'),
                                 labels = date_format("%b-%d\n%H:%M"),
                                 limits = c(range(na.omit(plot.data$datetime))[1],
                                            range(na.omit(plot.data$datetime))[2]))+
                scale_y_continuous(sec.axis = sec_axis(~(.-scale.parB)/scale.parA,
                                                       name="Fm' or <b style='color:#808080'>F'</b>"),
                                   name = bquote(atop(paste(Phi,'PSII [Y(II)]'),
                                                      .(j))))+
                theme_bw()+
                theme(legend.position = 'none',
                      title = element_text(size=12,color='black',face='bold'),
                      axis.text.y.left = element_text(size = 15,color = 'white'),
                      axis.text.y.right = element_text(size = 15,color = 'white'),

                      axis.text.x =  element_text(size = 15,color = 'black'),
                      axis.title.y.left = element_text(size = 16,color = 'forestgreen'),
                      axis.title.y.right = element_text(size = 16,color = 'black'),

                      axis.title.x =element_blank(),
                      axis.ticks.y =element_line(color = 'white'),
                      axis.line.y.left = element_line(color = 'forestgreen'),
                      plot.margin = unit(c(0,0,0,0),'cm'))
              p23.fc<-function(p23.color){
                ggplot(plot.data,aes(x=datetime))+
                  geom_line(aes(y=temp_PAM),
                            linewidth=0.1,color='white',na.rm = T)+
                  geom_line(aes(y=scale.parA*par_PAM+scale.parB),
                            linewidth=0.1,color='white',na.rm = T)+
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
                  sec.axis = sec_axis(~.,name=expression(paste('Temp ('^'o','C)'))))
              #plot third left-y-axis: PAR
              p3<-p23.fc('yellow3')+
                scale_y_continuous(
                  sec.axis = sec_axis(~(.-scale.parB)/scale.parA,
                                      name='PAR'))
              plot.PAM.figure<-p1 + p2 + p3 +
                plot_layout(nrow =1, widths = c(15, 0.1, 0.1))
              }
            }

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

              p1<-ggplot(plot.data,aes(x=datetime))+
                geom_rect(data=night,fill='lightskyblue1',na.rm = T,alpha=0.008,
                          aes(xmin=sunsetStart,xmax=sunriseDay2,
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
                  name = bquote(atop(paste(Phi,'PSII [Y(II)]'),
                                     .(j))))+
                theme_bw()+
                theme(legend.position = 'none',
                      title = element_text(size=12,color='black',face='bold'),
                      axis.text.y.left = element_text(size = 15,color = 'forestgreen'),
                      axis.text.y.right = element_text(size = 15,color = 'black'),
                      axis.text.x =  element_text(size = 15,color = 'black'),
                      axis.title.y.right  = element_text(size = 16),
                      axis.title.y.left = element_text(size = 16,color = 'forestgreen'),
                      axis.title.x =element_blank(),
                      axis.ticks.y.left =element_line(color = 'forestgreen'),
                      axis.line.y.left = element_line(color = 'forestgreen'),
                      plot.margin = unit(c(0,0,0,0),'cm'))

              if (isTRUE(fieldnote==TRUE)){

                if (isTRUE(length(na.omit(fieldnote.group$remark.plot))>0)) {

                  p1<-p1+
                    geom_point(data=na.omit(fieldnote.group),
                               aes(x=datetime,y=max(plot.data$YII,na.rm = T)-0.05),
                               color='red',size=4,na.rm=T)+
                    geom_text(data=na.omit(fieldnote.group),
                              aes(x=datetime,y=max(plot.data$YII,na.rm = T),
                                  label=remark.plot),color='red',size=3,na.rm = T)
                } else {
                  p1<-p1
                }
              } else {
                p1<-p1
              }

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
               height = 2*headlength.y+0.3)

      }
      #return(plot.all.title)
    }
  })
  print(Sys.time()-start.time)

}
