#' MONI-PAM seasonal parameters data visualization
#'
#' This function visualize MONI-PAM seasonal quenching (qLs,PQs, and NPQs) and quantum yield (FV/FM, Phi_NPQs, Phi(f,Ds)) parameters for each head.
#'
#' @usage plotSeasonPara(season.param,save.path)
#' @param season.param seasonal chlorophyll fluorescence parameters retrieved from [seasonalParams] function in this R package
#' @param save.path local folder for saving the plotted figures generated from this function
#' @importFrom lubridate ymd hour year month ymd_hms date day wday second isoweek yday week minute mday quarter
#' @importFrom data.table data.table setDT
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom ggtext element_markdown
#' @return [plotSeasonPara] will return/show the plotted seasonal quenching and yield parameters figures in the end, and save the plotted figures to your folder directly.
#' @export
#'
plotSeasonPara<-function(season.param,
                         save.path){
  season.param<-formatPAMdata(season.param)

  # plot quenching parameters
  plot.quench<-
    ggplot(season.param, aes(x = date)) +
    facet_grid(.~tree_num)+
    geom_line(aes(y=qLs*10),color='blue',size=1,na.rm = T,alpha=0.3)+
    geom_point(aes(y=qLs*10),color='blue',size=1,na.rm = T,alpha=0.3)+
    geom_line(aes(y=PQs),color='black',na.rm = T)+
    geom_point(aes(y=PQs),size=1,color='black',na.rm = T)+
    geom_line(aes(y=NPQs),color='red',na.rm = T)+
    geom_point(aes(y=NPQs),color='red',size=1,na.rm = T)+
    scale_x_date(breaks = date_breaks("3 month"), labels = date_format("%b-%d\n%Y"),
                 limits = c(range(na.omit(season.param$date))[1],
                            range(na.omit(season.param$date))[2]))+
    scale_y_continuous(sec.axis = sec_axis(~./10,name = "qLs"),
                       name = "PQs or <b style='color:#FF0000'>NPQs</b>")+
    theme_bw()+
    theme(legend.position = 'none',
          axis.text.y.left = element_text(size = 15,color = 'black'),
          axis.text.y.right= element_text(size = 15,color = 'blue'),

          axis.text.x =  element_text(size = 11,color = 'black'),
          axis.title.y.left  = element_markdown(size = 18,vjust = -0.5),
          axis.title.y.right  = element_markdown(size = 18,vjust = -0.5,color = 'blue'),
          axis.title.x =element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'),
          strip.text =element_blank(),
          strip.background = element_blank(),
          axis.ticks = element_line(color = 'black',linewidth = 1),
          panel.border = element_rect(color='black',linewidth = 1))

  # prepare seasonal-quantum-yield-parameter data
  season.yield<-
    season.param%>%
    select(date,head_tree,noonmean_PAR,nightmean_temp,FvFm, Phi_NPQs,Phi_fDs) %>%
    tidyr::pivot_longer(cols =c("FvFm","Phi_NPQs","Phi_fDs"),
                        names_to = 'Yield.name',
                        values_to  ='Yield.value') %>%
    mutate(Yield.name=as.factor(Yield.name)) %>%
    mutate(Yield.value=case_when(
      #Yield.value<0~0,
      #Yield.value>1~1,
      is.na(Yield.value)~0,
      TRUE~Yield.value)) %>%
    mutate(Yield.name=factor(Yield.name,levels=c('FvFm','Phi_NPQs','Phi_fDs')))
  # plot seasonal quantum yield parameters
  plot.yield<-
    season.yield %>%
    ggplot(aes(date,Yield.value,fill=Yield.name))+
    geom_area( size=1)+
    facet_wrap(~head_tree,nrow = 1)+
    scale_fill_manual(values = c('forestgreen','blue','red'),
                      labels=c(expression('F'[V]*'/'*'F'[M]),
                              expression(Phi*'NPQ'[S]),
                               expression(Phi*'f,D'[S])))+
    scale_x_date(breaks = '3 month', expand = c(0, 0),
                 labels = date_format("%b-%d\n%Y"))+
    scale_y_continuous(breaks = c(seq(0,1,0.2)), expand = c(0, 0))+
    theme_bw()+ylab('Yield')+
    theme(plot.title=element_text(hjust=0.5,size = 25),
          legend.position = 'top',
          legend.title=element_blank(),
          legend.text=element_text(color='black',size = 16),
          panel.grid = element_blank(),
          strip.text = element_text(color='black',size = 16),
          strip.background = element_blank(),
          axis.text.x = element_text(color='black',size = 14.5),
          axis.text.y = element_text(color='black',size = 16),
          axis.title.y = element_text(color='black',size = 20),
          axis.title.x = element_blank(),
          axis.ticks = element_line(color='black',linewidth=1),
          panel.background = element_rect(fill='grey80'),
          panel.border = element_rect(colour = 'black',linewidth = 1))

    headlength<-length(levels(season.param$head_tree))

    ggsave(plot.quench,
           filename = paste0(save.path,'/Seasonal quenching parameters ',
                             ' from ',range(na.omit(season.param$date))[1],
                             ' to ', range(na.omit(season.param$date))[2],'.tiff'),
           compression='lzw',dpi=100,height =2.7, width = 3*headlength)

    ggsave(plot.yield,
           filename = paste0(save.path,'/Seasonal yield parameters ',
                             ' from ',range(na.omit(season.param$date))[1],
                             ' to ', range(na.omit(season.param$date))[2],'.tiff'),
           compression='lzw',dpi=100,height = 3.5, width = 3*headlength)
    return(list(plot.quench,plot.yield))

}


