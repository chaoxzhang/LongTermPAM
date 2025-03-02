scaleFvFm<-function(PAM.fvfm){

  data<-PAM.fvfm %>% dplyr::select(-c(date,head,tree_num))

  # Define custom functions to handle Inf values
  safe_max <- function(x) {
    result <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.infinite(result)) NA else result
  }

  safe_min <- function(x) {
    result <- suppressWarnings(min(x, na.rm = TRUE))
    if (is.infinite(result)) NA else result
  }


  maxmin.res<-data %>%
    group_by(head_tree) %>%
    dplyr::summarise(
      across(
        .cols = c("noonmean_PAR","nightmean_temp","F0", "Fm","FvFm"),
        .fns = list(max = safe_max, min = safe_min),
        .names = "{.fn}.{.col}"
      ),
      .groups = 'drop'
    ) %>%
    dplyr::rename_with(~ str_replace(., ".noonmean_PAR", ".par")) %>%
    dplyr::rename_with(~ str_replace(., "nightmean_", ""))

  maxmin.res<-
    maxmin.res %>%
    mutate(scale.Fm.a=(max.FvFm-min.FvFm)/(max.Fm-min.Fm)) %>%
    mutate(scale.Fm.b=min.FvFm-scale.Fm.a*min.Fm) %>%
    mutate(scale.F0.a=(max.FvFm-min.FvFm)/(max.F0-min.F0)) %>%
    mutate(scale.F0.b=min.FvFm-scale.F0.a*min.F0) %>%
    mutate(scale.temp.a=(max.FvFm-min.FvFm)/(max.temp-min.temp)) %>%
    mutate(scale.temp.b=min.FvFm-scale.temp.a*min.temp) %>%
    mutate(scale.par.a=(max.FvFm-min.FvFm)/(max.par-min.par)) %>%
    mutate(scale.par.b= min.FvFm-scale.par.a*min.par)
    maxmin.res<-data.table(maxmin.res)

  maxmin.res[,c(12,14,16,18)][sapply(maxmin.res[,c(12,14,16,18)],is.na)]<-1
  maxmin.res[,c(13,15,17,19)][sapply(maxmin.res[,c(13,15,17,19)],is.na)]<-0

  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  return(maxmin.res)
}

#' MONI-PAM FV/FM data visualization
#'
#' This function will visualize MONI-PAM FV/FM data including F0,FM,FV/FM,PAR, and temperature in one figure for each head.
#'
#' @usage plotFVFM(PAM.fvfm,plot.title,save.path)
#' @param PAM.fvfm MONI-PAM FV/FM data retrieved from [FindFVFM] function in this R package
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
#' @return This function will not return/show the plotted figures in the end, instead, it will save the plotted figures to your local folder directly.
#' @export
plotFVFM<-function(PAM.fvfm,plot.title,save.path){

  start.time<-Sys.time()
  PAM.fvfm<-formatPAMdata(PAM.data=PAM.fvfm)

  extreme.data<-scaleFvFm(PAM.fvfm)

  plot.onetree<-

    lapply(levels(PAM.fvfm$head_tree),function(j){

      scale.data<-extreme.data %>% subset(head_tree==j) %>% droplevels()


      F0toFvFm<-function(x){scale.data$scale.F0.a*x+scale.data$scale.F0.b}
      FmtoFvFm<-function(x){scale.data$scale.Fm.a*x+scale.data$scale.Fm.b}
      TtoFvFm<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
      PARtoFvFm<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}


      plot.data<-PAM.fvfm %>% subset(head_tree==j) %>% droplevels()

      if (isTRUE(sum(na.omit(plot.data$FvFm))>0&
          length(na.omit(plot.data$FvFm))>2)) {#if data exist

        p1<-
          ggplot(plot.data,aes(x=date))+
          geom_line(aes(y=TtoFvFm(nightmean_temp)),linewidth=0.3,color='darkorchid',na.rm = T)+
          geom_line(aes(y=PARtoFvFm(noonmean_PAR)),linewidth=0.3,color='yellow3',na.rm = T)+
          geom_line(aes(y=F0toFvFm(F0)),color='orange',na.rm = T)+
          geom_point(aes(y=F0toFvFm(F0)),size=1,color='orange',na.rm = T)+
          geom_line(aes(y=FmtoFvFm(Fm)),color='black',na.rm = T)+
          geom_point(aes(y=FmtoFvFm(Fm)),color='black',size=1,na.rm = T)+
          geom_line(aes(y=FvFm),color='forestgreen',na.rm = T)+
          geom_point(aes(y=FvFm),size=1,color='forestgreen',na.rm = T)+
          scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%d"),
                       limits = c(range(na.omit(PAM.fvfm$date))[1],
                                  range(na.omit(PAM.fvfm$date))[2]))+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.data$scale.Fm.b)/scale.data$scale.Fm.a,
                                name=expression(paste('F'[M]))),
            name = bquote(atop(paste('F'[V],'/','F'[M]),
                               paste(.(j)))))+
          theme_bw()+
          theme(legend.position = 'none',
                title = element_text(size=12,color='black',face='bold'),
                axis.text.y.left = element_text(size = 15,color = 'forestgreen'),
                axis.text.y.right = element_text(size = 15,color = 'black'),
                axis.text.x =  element_text(size = 12,color = 'black'),
                axis.title.y.right  = element_text(size = 16,vjust = -0.5),
                axis.title.y.left = element_text(size = 14.5,color = 'forestgreen'),
                axis.title.x =element_blank(),
                axis.ticks.y.left =element_line(color = 'forestgreen'),
                axis.line.y.left = element_line(color = 'forestgreen'),
                plot.margin = unit(c(0,0,0,0),'cm'))

        # Function to create plots 2, 3, and 4 (2nd, 3rd, and 4th right y axis)
        p234.fc<-function(p234.color){
          ggplot(plot.data,aes(x=date))+
            geom_line(aes(y=TtoFvFm(nightmean_temp)),linewidth=0.1,color='white',na.rm = T)+
            geom_line(aes(y=PARtoFvFm(noonmean_PAR)),linewidth=0.1,color='white',na.rm = T)+
            geom_line(aes(y=FvFm),color='white',na.rm = T)+
            geom_point(aes(y=FvFm),size=1,color='white',na.rm = T)+
            geom_line(aes(y=F0toFvFm(F0)),color='white',na.rm = T)+
            geom_point(aes(y=F0toFvFm(F0)),size=1,color='white',na.rm = T)+
            geom_line(aes(y=FmtoFvFm(Fm)),color='white',na.rm = T)+
            geom_point(aes(y=FmtoFvFm(Fm)),color='white',size=1,na.rm = T)+
            theme_minimal()+
            theme(legend.position = 'none',
                  axis.text.y.right = element_text(size = 15,color = p234.color),
                  axis.text.y.left = element_blank(),
                  axis.line.y.right = element_line(color = p234.color),
                  axis.text.x =   element_blank(),
                  axis.title.y.right  = element_text(size = 16,color = p234.color,
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
            sec.axis = sec_axis(~(.-scale.data$scale.F0.b)/scale.data$scale.F0.a,
                                name=expression(paste('F'[0]))))
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
    title <- ggdraw() +
      draw_label(paste0(plot.title,
                        ' from ', range(na.omit(PAM.fvfm$date))[1],
                        ' to ',range(na.omit(PAM.fvfm$date))[2]),
                 x = 0.05,hjust = 0,fontface='bold')
    plot.all.title<-
      plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))
    daylength.x<-
      as.numeric(diff(range(na.omit(PAM.fvfm$date))))
    headlength.y<-length(plot.onetree[!sapply(plot.onetree,is.null)])
    ggsave(plot.all.title,
           filename = paste0(save.path,'/',plot.title,
                             ' from ',range(na.omit(PAM.fvfm$date))[1],
                             ' to ', range(na.omit(PAM.fvfm$date))[2],'.tiff'),
           compression='lzw',dpi=100,width = 0.04*daylength.x,
           height = 4*headlength.y)
    return(plot.all.title)
  }

  print(Sys.time()-start.time)

}
