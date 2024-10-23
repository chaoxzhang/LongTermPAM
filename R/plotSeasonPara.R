# ScaleSeasonPara is built for the purpose of plotting multiple right-y axis
ScaleSeasonPara<-function(season.param){

  data<-season.param %>% dplyr::select(c(head_tree,noonmean_PAR,
                                         nightmean_temp,FvFm,PQs,
                                         qLs,NPQs))
  maxmin.res<-data %>%
    group_by(head_tree) %>%
    dplyr::summarise(
      across(
        .cols = c("noonmean_PAR","nightmean_temp","FvFm",'PQs','qLs','NPQs'),
        .fns = list(max = ~ max(., na.rm = TRUE), min = ~ min(., na.rm = TRUE)),
        .names = "{.fn}.{.col}"
      ),
      .groups = 'drop'
    ) %>%
    dplyr::rename_with(~ str_replace(., ".noonmean_PAR", ".par")) %>%
    dplyr::rename_with(~ str_replace(., "nightmean_", ""))

  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA

  maxmin.res<-
    maxmin.res %>%
    mutate(scale.NPQ.a=(max.FvFm-min.FvFm)/(max.NPQs-min.NPQs)) %>%
    mutate(scale.NPQ.b=min.FvFm-scale.NPQ.a*min.NPQs) %>%
    mutate(scale.qLs.a=(max.FvFm-min.FvFm)/(max.qLs-min.qLs)) %>%
    mutate(scale.qLs.b=min.FvFm-scale.qLs.a*min.qLs) %>%
    mutate(scale.temp.a=(max.FvFm-min.FvFm)/(max.temp-min.temp)) %>%
    mutate(scale.temp.b=min.FvFm-scale.temp.a*min.temp) %>%
    mutate(scale.par.a=(max.FvFm-min.FvFm)/(max.par-min.par)) %>%
    mutate(scale.par.b= min.FvFm-scale.par.a*min.par)
  maxmin.res<-data.table(maxmin.res)

  maxmin.res[,c(14,16,18,20)][sapply(maxmin.res[,c(14,16,18,20)],is.na)]<-1
  maxmin.res[,c(15,17,19,21)][sapply(maxmin.res[,c(15,17,19,21)],is.na)]<-0

  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  return(maxmin.res)
}

#' MONI-PAM FV/FM data visualization
#'
#' This function will visualize MONI-PAM FV/FM data including F0,FM,FV/FM,PAR, and temperature in one figure each head.
#'
#' @usage plotSeasonPara(season.param,save.path,fieldnote,fieldnote.data)
#' @param season.param seasonal chlorophyll fluorescence parameters retrieved from [seasonalParams] function in this R package
#' @param save.path local folder for saving the plotted figures generated from this function
#' @param fieldnote TRUE or FALSE. If fieldnote = TRUE, fieldnote data should be assigned.
#' @param fieldnote.data a fieldnote file which can be such as .csv or .dat file but with a specific data format. You can check the example data from this package using data("fieldnote_2014_2015"). In this dataset, head and tree_num should be same with your MONI-PAM data, text in 'remark.plot' column will be shown in the plotted figure to show what has happened for that 'head'/ 'tree_num'/'head_tree' on which 'datetime' so it should be very short and clean. 'remark' column can can contain full description for remark.plot. 'manager' column shows who found that issue or conducted certain activity for that sensor (e.g., calibration)
#'
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom patchwork plot_layout
#' @importFrom cowplot ggdraw draw_label plot_grid
#' @importFrom ggtext element_markdown
#' @return [plotSeasonPara] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotSeasonPara<-function(season.param,
                         save.path,
                         fieldnote,
                         fieldnote.data){

  start.time<-Sys.time()
  season.param<-formatPAMdata(season.param)

  if (isTRUE(fieldnote==TRUE)){
    fieldnote.data=fieldnote.data
    fieldnote.data$date<-ymd(fieldnote.data$date,truncated = 3)
  }
  extreme.data<-ScaleSeasonPara(season.param)

  plot.onetree<-

    lapply(levels(season.param$head_tree),function(j){

      scale.data<-extreme.data %>% subset(head_tree==j) %>% droplevels()


      NPQtoFvFm<-function(x){scale.data$scale.NPQ.a*x+scale.data$scale.NPQ.b}
      qLStoFvFm<-function(x){scale.data$scale.qLs.a*x+scale.data$scale.qLs.b}
      TtoFvFm<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
      PARtoFvFm<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}


      plot.data<-season.param %>% subset(head_tree==j) %>% droplevels()

      if (isTRUE(sum(na.omit(plot.data$FvFm))>0&
                 length(na.omit(plot.data$FvFm))>2)) {#if data exist

        p1<-
          ggplot(plot.data,aes(x=date))+
          geom_line(aes(y=TtoFvFm(nightmean_temp)),linewidth=0.3,color='darkorchid',na.rm = T,alpha=0.5)+
          geom_line(aes(y=PARtoFvFm(noonmean_PAR)),linewidth=0.3,color='yellow3',na.rm = T,alpha=0.5)+
          geom_line(aes(y=qLStoFvFm(qLs)),color='orange',na.rm = T)+
          geom_point(aes(y=qLStoFvFm(qLs)),size=1,color='orange',na.rm = T)+
          geom_line(aes(y=NPQtoFvFm(NPQs)),color='darkred',na.rm = T)+
          geom_point(aes(y=NPQtoFvFm(NPQs)),color='darkred',size=1,na.rm = T)+
          geom_line(aes(y=NPQtoFvFm(PQs)),color='black',na.rm = T)+
          geom_point(aes(y=NPQtoFvFm(PQs)),color='black',size=1,na.rm = T)+
          geom_line(aes(y=FvFm),color='forestgreen',na.rm = T)+
          geom_point(aes(y=FvFm),size=1,color='forestgreen',na.rm = T)+
          scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%d"),
                       limits = c(range(na.omit(season.param$date))[1],
                                  range(na.omit(season.param$date))[2]))+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.data$scale.NPQ.b)/scale.data$scale.NPQ.a,
                                name="PQs or <b style='color:#8B0000'>NPQs</b>"),
            name = bquote(atop(paste('F'[V],'/','F'[M]),
                               paste(.(j)))))+
          theme_bw()+
          theme(legend.position = 'none',
                title = element_text(size=12,color='black',face='bold'),
                axis.text.y.left = element_text(size = 17,color = 'forestgreen'),
                axis.text.y.right = element_text(size = 17,color = 'black'),
                axis.text.x =  element_text(size = 12,color = 'black'),
                axis.title.y.right  = element_markdown(size = 18,vjust = -0.5),
                axis.title.y.left = element_text(size = 18,color = 'forestgreen'),
                axis.title.x =element_blank(),
                axis.ticks.y.left =element_line(color = 'forestgreen'),
                axis.line.y.left = element_line(color = 'forestgreen'),
                plot.margin = unit(c(0,0,0,0),'cm'))
        if (isTRUE(fieldnote==T)){

          fieldnote.group<-
            fieldnote.data %>%
            subset(head_tree==j,select = c('date','head_tree','remark.plot')) %>%
            subset(remark.plot!='endSeason') %>%
            na.omit() %>% droplevels()

          if (isTRUE(length(na.omit(fieldnote.group$remark.plot))>0)) {# when there is fieldnote for this observation season
            p1<-p1+
              geom_point(data=fieldnote.group,
                         aes(x=date,y=max(plot.data$FvFm,na.rm = T)-0.05),
                         color='red',size=3,na.rm = T)+
              geom_text(data=fieldnote.group,
                        aes(x=date,y=max(plot.data$FvFm,na.rm = T),
                            label=remark.plot),color='red',size=3,na.rm = T)
          } else {
            p1<-p1
          }
        } else {
          p1<-p1
        }
        # Function to create plots 2, 3, and 4 (2nd, 3rd, and 4th right y axis)
        p234.fc<-function(p234.color){
          ggplot(plot.data,aes(x=date))+
            geom_line(aes(y=TtoFvFm(nightmean_temp)),linewidth=0.1,color='white',na.rm = T)+
            geom_line(aes(y=PARtoFvFm(noonmean_PAR)),linewidth=0.1,color='white',na.rm = T)+
            geom_line(aes(y=FvFm),color='white',na.rm = T)+
            geom_point(aes(y=FvFm),size=1,color='white',na.rm = T)+
            geom_line(aes(y=qLStoFvFm(qLs)),color='white',na.rm = T)+
            geom_point(aes(y=qLStoFvFm(qLs)),size=1,color='white',na.rm = T)+
            geom_line(aes(y=NPQtoFvFm(NPQs)),color='white',na.rm = T)+
            geom_point(aes(y=NPQtoFvFm(NPQs)),color='white',size=1,na.rm = T)+
            geom_line(aes(y=NPQtoFvFm(PQs)),color='white',na.rm = T)+
            geom_point(aes(y=NPQtoFvFm(PQs)),color='white',size=1,na.rm = T)+
            theme_minimal()+
            theme(legend.position = 'none',
                  axis.text.y.right = element_text(size = 17,color = p234.color),
                  axis.text.y.left = element_blank(),
                  axis.line.y.right = element_line(color = p234.color),
                  axis.text.x =   element_blank(),
                  axis.title.y.right  = element_text(size = 18,color = p234.color,
                                                     vjust = -0.5),
                  axis.title.y.left = element_blank(),
                  axis.title.x =element_blank(),
                  axis.ticks.y.left = element_blank(),
                  axis.ticks.y.right =element_line(color = p234.color),
                  panel.grid=element_blank(),
                  plot.margin = unit(c(0,0,0,0),'cm'))
        }
        p2<-p234.fc('orange')+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.data$scale.qLs.b)/scale.data$scale.qLs.a,
                                name=expression(paste('qLs'))))
        p3<-p234.fc('darkorchid')+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.data$scale.temp.b)/scale.data$scale.temp.a,
                                name=expression(paste('Night mean T ('^'o','C)'))))
        p4<-p234.fc('yellow3')+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.data$scale.par.b)/scale.data$scale.par.a,
                                name=expression(paste('Noon mean PAR (µmol m'^'-2',' s'^'-1',')'))))

        plot.PAM.figure <- p1 + p2 + p3 + p4  +
          plot_layout(nrow =1, widths = c(15, 0.1, 0.1, 0.1, 0.1))
      }
    }
    )

  if (isTRUE(length(plot.onetree[!sapply(plot.onetree,is.null)])>0)){
    plot.all<-
      plot_grid(plotlist = plot.onetree[!sapply(plot.onetree,is.null)],
                align = 'hv',ncol = 1)
    title <- ggdraw() +theme(plot.margin = margin(0,0,0,0))+
      draw_label(paste0('Seasonal parameters ',
                        ' from ', range(na.omit(season.param$date))[1],
                        ' to ',range(na.omit(season.param$date))[2]),
                 x = 0.05,hjust = 0,vjust=2,fontface='bold',size=18)
    plot.all.title<-
      plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))
    headlength.y<-length(plot.onetree[!sapply(plot.onetree,is.null)])
    ggsave(plot.all.title,
           filename = paste0(save.path,'/Seasonal parameters ',
                             ' from ',range(na.omit(season.param$date))[1],
                             ' to ', range(na.omit(season.param$date))[2],'.tiff'),
           compression='lzw',dpi=100,width = 15,
           height = 4*headlength.y)
    return(plot.all.title)
  }

  print(Sys.time()-start.time)

}
