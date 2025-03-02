#' MONI-PAM data visualization with data before and after filter
#'
#' This function visualizes two MONI-PAM datasets - one before filtering (dataBeforeFilter) and one after filter (dataAfterFilter) - including F',FM',Y(II),PAR, and temperature, grouped into 10-day plots for each head. It also displays sunlight times as vertical lines: sunrise and sunsetStart ('indianred'), solarNoon ('goldenrod'), and dawn/dusk ('purple'). In the figure, red, pink, and orange points means filtered or flagged Y(II), Fm', and F' data in data after filter compared to data before filter.
#'
#' @usage plotCheckFilter(dataAfterFilter, dataBeforeFilter,save.path)
#' @param dataAfterFilter one of MONI-PAM data after data filter (e.g., output file from filters 1 to 6 functions, such as [filter3.day] function)
#' @param dataBeforeFilter original organized and combined MONI-PAM data which is generated after [readPAM] function or other previous filtered data such as output data from [filter2.night] function.
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
#' @return [plotCheckFilter] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotCheckFilter<-function(dataAfterFilter,
                           dataBeforeFilter,
                           save.path
                          ){
  start.time<-Sys.time()

  newdata<-formatPAMdata(PAM.data=dataAfterFilter)
  rawdata<-formatPAMdata(PAM.data=dataBeforeFilter)


  extreme.data<-scalePAMdata(rawdata)

  lapply(levels(rawdata$plot.group),function(i){

    rawdata.onegroup<-
      droplevels(rawdata[rawdata$plot.group==i,])

    if (isTRUE(sum(na.omit(rawdata.onegroup$YII))>0&
        length(na.omit(rawdata.onegroup$YII))>2)){

      newdata.onegroup<-
        droplevels(newdata[newdata$plot.group==i,])


      if (isTRUE('flag.all' %in% colnames(rawdata.onegroup))){
        flag.data<-rawdata.onegroup

        flag.data<-
          merge(newdata.onegroup[,c("datetime",'head_tree','flag.all')],
                rawdata.onegroup[,-c('flag.all')],
                by=c("datetime",'head_tree'),all=T)

      } else {

        flag.data<-
          merge(newdata.onegroup[,c("datetime",'head_tree','flag.all')],
                rawdata.onegroup,by=c("datetime",'head_tree'),all=T)
      }

      flag.data0<-flag.data[flag.data$flag.all==0,]
      flag.data2<-newdata.onegroup[which((!is.na(newdata.onegroup$Fm_))& # there is Fm' value but YII is flagged
                                           newdata.onegroup$flag.all==0),]
      scaledata.onegroup<-
        droplevels(extreme.data[extreme.data$plot.group==i,])

      plot.onegroup<-
        lapply(levels(rawdata.onegroup$head_tree),function(j){
          scale.data<-
            droplevels(scaledata.onegroup[scaledata.onegroup$head_tree==j,])

          FtoYII<-function(x){scale.data$scale.Fm.a*x+scale.data$scale.Fm.b}
          TtoYII<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
          PARtoYII<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}


          top.data0<-
            droplevels(flag.data0[flag.data0$head_tree==j,])
          top.data2<-
            droplevels(flag.data2[flag.data2$head_tree==j,])
          bottom.data<-
            droplevels(rawdata.onegroup[rawdata.onegroup$head_tree==j,])

          if (isTRUE((sum(na.omit(bottom.data$YII))>0&
                                 length(na.omit(bottom.data$YII))>2))) {

            night<-unique(bottom.data[,c('date','sunsetStart','sunrise')])
            night<-night[order(night$sunsetStart),]

            if (nrow(night)>1){
              night$sunriseDay2<-ymd_hms(night$sunrise[2:nrow(night)],NA)
              } else if(nrow(night)==1){
                night$sunriseDay2<-night$sunrise+3600*24
              }

            night<-merge(bottom.data[,c('datetime','date')],night,by='date',all=T)
            night.day1<-bottom.data %>% subset(date==min(date,na.rm = T)&datetime<=sunrise)
            night.lastDay<-bottom.data %>% subset(date==max(date,na.rm = T)&datetime>=sunsetStart)

            p1<-ggplot(bottom.data,aes(x=datetime))+
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
              geom_point(aes(y=FtoYII(F_)),size=1,color='grey30',na.rm = T)+
              geom_point(data=top.data0,aes(y=FtoYII(F_)),size=1,color='orangered',na.rm = T)+
              geom_point(data=top.data2,aes(y=FtoYII(F_)),size=1,color='grey30',na.rm = T)+
              geom_line(aes(y=FtoYII(Fm_)),color='black',na.rm = T)+
              geom_point(aes(y=FtoYII(Fm_)),color='black',size=1,na.rm = T)+
              geom_point(data=top.data0,aes(y=FtoYII(Fm_)),color='deeppink',size=1,na.rm = T)+
              geom_point(data=top.data2,aes(y=FtoYII(Fm_)),color='black',size=1,na.rm = T)+
              geom_line(aes(y=YII),color='forestgreen',na.rm = T)+
              geom_point(aes(y=YII),size=1,color='forestgreen',na.rm = T)+
              geom_point(data=top.data0,aes(y=YII),size=1,color='red2',na.rm = T)+
              scale_x_datetime(breaks = date_breaks("1 day"),
                               labels = date_format("%b-%d\n%H:%M"),
                               limits = c(range(na.omit(rawdata.onegroup$datetime))[1],
                                          range(na.omit(rawdata.onegroup$datetime))[2]))+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.Fm.b)/scale.data$scale.Fm.a,
                                    name="Fm' or <b style='color:#808080'>F'</b>"),
                name = "<b style='color:forestgreen'>&#x03A6;P [Y(II)]</b>")+
              theme_bw()+
              theme(legend.position = 'none',
                    strip.text = element_text(size = 14,color = 'black',face='bold'),
                    strip.placement = 'outside',
                    strip.background = element_blank(),
                    axis.text.y.left = element_text(size = 15,color = 'forestgreen'),
                    axis.text.y.right = element_text(size = 15,color = 'black'),
                    axis.text.x =  element_text(size = 15,color = 'black'),
                    axis.title.y.right  = element_markdown(size = 16),
                    axis.title.y.left = element_markdown(size = 16,color = 'forestgreen'),
                    axis.title.x =element_blank(),
                    axis.ticks.y.left =element_line(color = 'forestgreen'),
                    axis.line.y.left = element_line(color = 'forestgreen'),
                    plot.margin = unit(c(0,0,0,0),'cm'))

            p23.fc<-function(p23.color){
              ggplot(bottom.data,aes(x=datetime))+
              geom_line(aes(y=TtoYII(temp_PAM)),linewidth=0.1,color='white',na.rm = T)+
              geom_line(aes(y=PARtoYII(par_PAM)),linewidth=0.1,color='white',na.rm = T)+
              geom_line(aes(y=YII),color='white',na.rm = T)+
              geom_point(aes(y=YII),size=0.5,color='white',na.rm = T)+
              geom_point(data=top.data0,aes(y=YII),size=0.5,color='white',na.rm = T)+
              geom_line(aes(y=FtoYII(F_)),color='white',na.rm = T)+
              geom_point(aes(y=FtoYII(F_)),size=0.5,color='white',na.rm = T)+
              geom_point(data=top.data0,aes(y=FtoYII(F_)),size=0.5,color='white',na.rm = T)+
              geom_point(data=top.data2,aes(y=FtoYII(F_)),size=0.5,color='white',na.rm = T)+
              geom_line(aes(y=FtoYII(Fm_)),color='white',na.rm = T)+
              geom_point(aes(y=FtoYII(Fm_)),color='white',size=0.5,na.rm = T)+
              geom_point(data=top.data0,aes(y=FtoYII(Fm_)),color='white',size=0.5,na.rm = T)+
              geom_point(data=top.data2,aes(y=FtoYII(Fm_)),color='white',size=0.5,na.rm = T)+
              theme_minimal()+
              theme(legend.position = 'none',
                    axis.text.y.left = element_blank(),
                    axis.text.x =   element_blank(),
                    axis.title.y.left = element_blank(),
                    axis.title.x =element_blank(),
                    axis.ticks.y.left = element_blank(),
                    panel.grid=element_blank(),
                    plot.margin = unit(c(0,0,0,0),'cm'),
                    axis.text.y.right = element_text(size = 15,color = p23.color),
                    axis.line.y.right = element_line(color = p23.color),
                    axis.title.y.right  = element_text(size = 16,color = p23.color),
                    axis.ticks.y.right =element_line(color = p23.color))
            }

            p2<-p23.fc('darkorchid')+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.temp.b)/scale.data$scale.temp.a,
                                    name=expression(paste('Temperature ('^'o','C)'))))
            p3<-p23.fc('yellow3')+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.par.b)/scale.data$scale.par.a,
                                    name='PAR'))
            print(paste0("plot.group[",i,"]:",j," is plotted"))
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
          draw_label(paste0('Filtered data from ',
                            range(na.omit(rawdata.onegroup$date))[1],
                            ' to ',
                            range(na.omit(rawdata.onegroup$date))[2],
                            ' after ',names(newdata)[ncol(newdata)-1]),
                     x = 0.05,hjust = 0,fontface='bold',size=20)+
          theme(plot.margin = margin(0,0,0,0))

        plot.all.title<-
          plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))

        daylength.x<-as.numeric(diff(range(na.omit(rawdata.onegroup$date))))
        headlength.y<-length(plot.onegroup[!sapply(plot.onegroup,is.null)])
        ggsave(plot.all.title,
               filename = paste0(save.path,'/','Filtered data from ',
                                 range(na.omit(rawdata.onegroup$date))[1],
                                 ' to ',
                                 range(na.omit(rawdata.onegroup$date))[2],
                                 ' after ',names(newdata)[ncol(newdata)-1],'.tiff'),
               compression='lzw',dpi=100,width = 1.5*daylength.x+6,
               height = 2.3*headlength.y+0.3)


      }
      #return(plot.all.title)
    }

  })
  Sys.time()-start.time
}
