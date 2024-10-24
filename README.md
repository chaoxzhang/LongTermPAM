---
title: "Introduction to LongTermPAM"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to LongTermPAM}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  markdown: 
    wrap: 72
---

```{r, include = FALSE}
# This chunk only includes setting for this R markdown, please do not run scripts in this chunk
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(eval = FALSE)
```

**To test the code and clean your own dataset, the user can download the Rmarkdown file "Intro_To_LongTermPAM.Rmd" to the local directory from here: "https://github.com/chaoxzhang/LongTermPAM/tree/main/vignettes/". The example dataset inside this Rmd file is located at: "https://github.com/chaoxzhang/LongTermPAM/tree/main/inst/extdata". To process your own dataset, you can replace the example dataset to your own dataset using this Rmarkdown file.**

# Install LongTermPAM package using:

```{r}
#remotes::install_github("chaoxzhang/LongTermPAM")
```

```{r}
# load LongTermPAM package
library(LongTermPAM)

# check the work directory location
getwd() 
```

# 1. organize and pre-process MONI-PAM data

## 1.1 read and organize MONIPAM data: readPAM function

before MONIPAM data cleaning (data filtering), we need organize MONIPAM
data and make them easily read in R

```{r}
#' # check the data folder location measured in 2014-2015 observation season
#' data20142015.path<-system.file('extdata/2014-2015',package='LongTermPAM')
#' # list all the files inside 2014-2015 folder
#' list.files(data20142015.path)
#'
#' # check the data folder location measured in 2016-2017 observation season
#' data20162017.path<-system.file('extdata/2016-2017',package='LongTermPAM')
#' # list all the files inside 2016-2017 folder
#' list.files(data20162017.path)
#'
#' #MONI-PAM data can be checked by using basic R functions, for example
#' filename<-list.files(data20162017.path,full.names=T)
#' #check first MONI-PAM file inside 2016-2017 folder
#' test1<-read.delim(filename[1])
```

```{r}
PAM2014.2015<-
  readPAM(source.path=system.file("extdata/2014-2015/",package = "LongTermPAM"),
               pam.pattern = '.PAM',
               save.path = getwd(),# it is better to modify this to your local path
               site.lat =61.84562, # hyytiala latitude
               site.lon = 24.29077, # hyytiala longitude
               local.tz = 'EET', # time zone of Finland
               tz.summer =  3,
               tz.winter = 2,
               measure.time = 'winter' # In Hyytiala forest station, we always use winter time
               )
```

readPAM() function saves (a) well organized each single original '.PAM'
data to '.csv' extension as you can see during the running of the
function, and (b) generates three '.dat' files respectively, including:

-   **rawPAM_2014_2015.dat** This is a combined original PAM data

-   **preprocesPAM_2014_2015.dat**\
    This file combined all the organized data and this is the file used
    for further data processing and cleaning. In this file, columns
    "plot.group", "dateBack12h",
    datetimeBack12h","sunrise","sunriseEnd","solarNoon", "sunsetStart",
    "sunset", and "dusk" were added for later easily data cleaning and
    data visualization purpose.

-   **preprocesPAM_head_timerange_2014_2015.dat** this file only
    contains 4 columns:

    -   "head": MONI-HEAD number
    -   "tree_num"
    -   "start.date"(start measuring time of this MONI-head and
        tree_num)
    -   "end.date" (end measuring time of this MONI-head and tree_num)

    By checking this file, we can know if there is a mistake in head
    and/or tree_num. If there is a mistake, we need modify the head
    and/or tree_num information in **preprocesPAM_2014_2015.dat**
    according this file, in order to correctly further data management,
    cleaning and analysis

## 1.2 correct head and/or tree_num information if needed

Therefore, let's check the head and tree_num information by reading
**preprocesPAM_head_timerange_2014_2015.dat** into R

```{r}
#read the head information into R, remember your saved-data path
head.timerange<-read.csv("preprocesPAM_head_timerange_2014_2015.dat",header = T,sep = ';')
#print this head and tree information
head.timerange
```

In this example, we can see that after 2015-08-28 06:03:45, all the
tree_num were given wrong names. Therefore, we need to correct them in
the combined and well organized file **preprocesPAM_2014_2015.dat**
correspondingly

Note that the time format (columns of start.data and end.data) is same
with original measurement settings and usually same with all the other
measurements in the same study site. Here, they were using winter time
zone for observation period in Finland.

```{r}
# read preprocesPAM_2014_2015.dat into R using fread from data.table package
preproces.20142015<-fread('preprocesPAM_2014_2015.dat')

# arrange the preproces.20142015 data to correct data format using formatPAMdata function from this package
preproces.20142015<-formatPAMdata(preproces.20142015)

# correct tree_num
preproces.20142015<-
  preproces.20142015 %>%
  #tree_num is confirmed by checking field-note data of measuring season 2014-2015
  #see: data(fieldnote_for_plot)
  mutate(tree_num=case_when(
    head==6~'T7TOP',
    head==11~'T7LOW',
    head==14~'T6LOW',
    head==22~'T5TOP',
    head==25~'T6TOP'
  )) %>%
  mutate(tree_num=as.factor(tree_num)) %>% 
  mutate(head_tree=paste0(head,'_',tree_num))

# check tree_num
levels(preproces.20142015$tree_num)
# check data
head(preproces.20142015)
```

## 1.3 remove first few days data

To make sure that the first data we start to use/clean is good
measurement, we usually remove first several days data. Because, usually
after inserting the leaves/needles into MONI-PAM clips, the
leaves/needles need a time to adjust themselves to adapt the new growing
condition (with clip). Therefore, the data collected from the beginning
might be wrong and it is better to remove them before we start our real
data cleaning and further data analysis.

NOTE that You can check the original data using WinControl software
before deciding which days you want to remove since they are all quite
good, here we just remove data from first day measurement

**Here we remove the data before 2014-08-28 12:00 after checking the
data using WinControl software**

```{r}
preproces.20142015<-
  preproces.20142015 %>% 
  subset(datetime>ymd_hms('2014-08-28 12:00:00'))

# check the data now
View(preproces.20142015)
```

To avoid the mistake occurred in future, we recommend you always to save
the modified data into a new file for further data cleaning and keep
this original combined file **preprocesPAM_2014_2015.dat** without
further modification. Then you can easily come back to check them from
this original combined file **preprocesPAM_2014_2015.dat** .

## 1.4 correct F if needed

Temperature dependency of ChlF correction. The correction parameters
were given default value but can also be adjusted. Details check helps
of correctF() function

```{r}
preproces.20142015.correctF<-correctF(preproces.20142015)
```

## 1.5 save the modified/corrected file

```{r}
# write.table function from R
# I call it corrected data, because usually we need correct head and tree info for our Hyytiala data
write.table(preproces.20142015.correctF,file='corrected2014-2015.dat',row.names = F,sep = ';')
```

# 2. data cleaning

## 2.1. read organized and corrected MONIPAM data for data cleaning

```{r}
# This is only an example, you can decide where the cleaned/filtered data and visualized figure will be saved 

# Function to create a directory if it doesn't exist
create_dir_if_not_exists <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)  # recursive = TRUE ensures that any necessary parent directories are created
  }
}

# Create a folder for saving all the data
create_dir_if_not_exists('saved data')

# Create a new folder to save data visualization figures
create_dir_if_not_exists('figures')

# Create subfolders to save different data visualization figures
create_dir_if_not_exists('figures/figure_raw_data')
create_dir_if_not_exists('figures/figure_filter_step')
create_dir_if_not_exists('figures/figure_diurnal_parameters')
create_dir_if_not_exists('figures/figure_seasonal_parameters')
```

```{r}
fieldnote<-fieldnote_2014_2015
save.rawdataFigure.path<-'figures/figure_raw_data'
save.fitlerFigure.path<-'figures/figure_filter_step'
save.filterData.path<-'saved data'
save.seasonalFigure.path<-'figures/figure_seasonal_parameters'
```

```{r}
# fread function from data.table package

flag0<-fread("corrected2014-2015.dat",stringsAsFactors = T)


# first, let's see how fvfm from original data varies across whole observing season
# calculate FvFm using FindFvFm and check data by plotting fvfm data

flag0.fvfm<-  FindFvFm(PAM.data=flag0,save.path = save.filterData.path,
                       save.title = 'Flag 0',save.file=T)

# plot and save to local folder
plotFvFm(flag0.fvfm, plot.title = 'Flag 0 fvfm', fieldnote=T,fieldnote.data = fieldnote,
         save.path=save.seasonalFigure.path)

# visulize original data  using plotRawData() function 
plotRawData(flag0, fieldnote =  T,fieldnote.data = fieldnote,plot.title = 'with sunlight time',save.path = save.rawdataFigure.path)
```

**cleaning data step 1**

```{r}
# filter1.lowF
flag1.lowF<-filter1.lowF(PAM.data=flag0,save.path=save.filterData.path,
                     save.file=T)
#check fvfm after filter step 1
flag1.fvfm<-FindFvFm(flag1.lowF, save.filterData.path,'Flag 1',save.file=T)
# plot and save fvfm figure to local folder
plotFvFm(flag1.fvfm,plot.title = 'Flag 1 fvfm',fieldnote = T,fieldnote.data = fieldnote,
         save.path = save.seasonalFigure.path)
#visualize data filtering
plotCheckfilter(newdata=flag1.lowF,rawdata=flag0,fieldnote = T,
                fieldnote.data = fieldnote_2014_2015,
                save.path=save.fitlerFigure.path)
```

**continue for steps 2 to 6**

for steps 2 to 6, you will have different threshold parameters that need
to be adjusted according to how the filer performance will be done by
visually checking the cleaned data (figure output from plotCheckfilter
function). This is why there is always plotFv/Fm() and plotCheckfilter()
function in the end of each filter step to visualize your data filtering
results.

plotFvFm() to check whole growing season data filtering results and
plotCheckfilter() to check day by day

Refine these parameters until you feel your filter is good enough by
checking plotted diurnal figures, then move to next filter step

```{r}
# adjust f2.fm from 0.01 to 0.1 by a 0.01 interval
# fmYII = 3 usually is a good setting, you can just try to adjust f2.fm
flag2.night<-
   filter2.night(flag1.lowF,f2.fm=0.05,f2.fmYII=3,
                 save.path=save.filterData.path,save.file = T)
flag2.fvfm<-
  FindFvFm(flag2.night, save.filterData.path,'Flag 2',save.file=T)
plotFvFm(flag2.fvfm,plot.title = 'Flag 2 fvfm',save.path = save.seasonalFigure.path,
         fieldnote = T,fieldnote.data = fieldnote)
plotCheckfilter(newdata=flag2.night,rawdata=flag0,fieldnote = T,
                save.path=save.fitlerFigure.path,fieldnote.data = fieldnote)
```

```{r}
# adjust f3.YII from 0.01 to 0.1 by a 0.01 interval
# adjust f3.fm from 0.05 to 0.3 by a 0.05 interval
# fmYII = 3 usually is a good setting, you can just try to adjust f3.YII and f3.fm 
flag3.day<-
   filter3.day(flag2.night,f3.YII=0.02,f3.fm=0.1,f3.fmYII = 3,
                save.path =save.filterData.path,save.file = T)
flag3.fvfm<-
  FindFvFm(flag3.day, save.filterData.path,'Flag 3',save.file=T)

plotFvFm(flag3.fvfm,plot.title = 'Flag 3 fvfm',save.seasonalFigure.path,
         fieldnote = T,fieldnote.data = fieldnote)

plotCheckfilter(newdata=flag3.day,rawdata=flag0,fieldnote = T,
                save.path=save.fitlerFigure.path,fieldnote.data = fieldnote)
```

```{r}
# adjust f4.fm from 0.05 to 0.3 by a 0.05 interval
# f4.fmYII = 3 usually is a good setting, you can just try to adjust f4.fm
flag4.FvFm<-
   filter4.FvFm(flag3.day,save.file = T,f4.fm=0.15,f4.fmYII=3,
                    save.path = save.filterData.path)
flag4.fvfm<-
  FindFvFm(flag4.FvFm,save.filterData.path,'Flag 4',save.file=T)

plotFvFm(flag4.fvfm,plot.title = 'Flag 4 fvfm',save.seasonalFigure.path,
         fieldnote = T,fieldnote.data = fieldnote)
plotCheckfilter(newdata=flag4.FvFm,rawdata=flag0,fieldnote=T,
                save.path=save.fitlerFigure.path,fieldnote.data = fieldnote)
```

```{r}
# adjust f5.fm from 0.1 to 0.3 by a 0.05 interval
# f5.fmYII = 3 usually is a good setting, you can just try to adjust f5.fm
flag5.expand<-filter5.expand(flag4.FvFm,save.file = T,
                           save.path = save.filterData.path,
                            f5.fm = 0.2,f5.fmYII = 3)
flag5.fvfm<-
  FindFvFm(flag5.expand,save.filterData.path,'Flag 5',save.file=T)


plotFvFm(flag5.fvfm,plot.title = 'Flag 5 fvfm',save.seasonalFigure.path,
         fieldnote = T,fieldnote.data = fieldnote)
plotCheckfilter(newdata=flag5.expand,rawdata=flag0,fieldnote = T,
                save.path=save.fitlerFigure.path,fieldnote.data = fieldnote)
```

```{r}
# adjust expand.time  according your data recording interval, 60 mins here for this example include 2 measurement points

flag6.adjacent<-filter6.adjacent(flag5.expand,save.file = T,
                          save.path = save.filterData.path,
                          expand.time = 60)
flag6.fvfm<-
  FindFvFm(flag6.adjacent,save.filterData.path,'Flag 6',save.file=T)

plotFvFm(flag6.fvfm,plot.title = 'Flag 6 fvfm',save.seasonalFigure.path,
         fieldnote = T,fieldnote.data = fieldnote)
plotCheckfilter(newdata=flag6.adjacent,rawdata=flag0,fieldnote=T,
                save.path=save.fitlerFigure.path,fieldnote.data = fieldnote)
```

# 3. calculate other paramters

## 3.1 estimate FmR and FoR using ChlFRef function

To calculate other chlorophyll fluorescence parameters, we firstly need
to know Fm reference (FmR) and Fo reference (FoR) when there is no NPQ
or NPQ is near 0. In this case, Fv/Fm will be maximum value for whole
observation period. For example, Scots pine needles usually have maximum
Fv/Fm (FvFmR) around 0.84. Additionally, Fv/Fm and Fm usually have a
clear similar seasonal pattern. Therefore, We built a non-linear
regression (exponential) model between Fm and Fv/Fm: Fm\~a \* exp(b \*
Fv/Fm) to estimate FmR based on FvFmR. You can define FvFmR value based
on the species you measured. The corresponding FoR then can be estimated
as: FoR= (1-FvFmR)\*FmR

The output of ChlFRef function include a data.frame which has variables
of FmR and FoR for each measuring head and also includes the summary
(R2, bias,RMSE,and RRMSE) of non-linear regression model simulation
between Fm and Fv/Fm data from entire observation period. The purpose of
including the summary of the model simulation is to evaluate the
filtering performance for Fv/Fm. All the output data is also visualized
through ChlFRef function.

**NOTE that you can also define FmR and FoR yourself instead of using
ChlFRef function here**. If you will continue to use this package to
calculate other chlorophyll fluorescence parameters, FmR data you
provided yourself should be a data.frame or data.table and include
columns: 'season','head_tree','head','tree_num','FmR' and 'FoR'.

```{r}
#flag0.fvfm<-fread('saved data/PAM_FvFm_Flag 0_2014_2015.dat')
#flag6.fvfm<-fread('saved data/PAM_FvFm_Flag 6_2014_2015.dat')
#flag0<-fread("corrected2014-2015.dat",stringsAsFactors = T)
#flag6.adjacent<-fread("saved data/PAM_2014_2015_filter6adjacent.dat",stringsAsFactors = T)
```

```{r fig.height=6,fig.width=20}
FmR.df<-ChlFRef(raw.fvfm = flag0.fvfm,filter.fvfm = flag6.fvfm,FvFmR = 0.83,
                save.file = T,save.path = 'figures/figure_FmR_FoR')
FmR.df
```

## 3.2 estimate diurnal parameters using diurnalParams function

This function will estimate diurnal parameters,including rate constant
parameters: PQ, NPQ,NPQr; quenching parameters: qLT,qLr, and quantum
yield parameters: Phi_NPQ, Phi_NPQr,Phi_NPQs, Phi_fD. Please see
calculation method and parameter description in Porcar-Castell et al.
2011 and Zhang et al. XX

```{r}
diurn.para<-diurnalParams(filtered.data = flag6.adjacent,
                          filtered.fvfm = flag6.fvfm,
                          # only the FmR and FoR estimated from final filtered
                          # data will be used to calculate ChlF parameters
                          FmR.data = FmR.df %>% subset(var=='Filtered data'),
                          save.file = T,save.path = 'saved data')

names(diurn.para)
```

## 3.3 estimate seasonal parameters using seasonalParams function

This function will estimate diurnal parameters,including rate constant
parameters: PQ, NPQ,NPQr; quenching parameters: qLT,qLr, and quantum
yield parameters: Phi_NPQ, Phi_NPQr,Phi_NPQs, Phi_fD. Please see
calculation method and parameter description in Porcar-Castell et al.
2011 and Zhang et al. XX

```{r}
seas.para<-seasonalParams(filtered.fvfm = flag6.fvfm,
                           # only the FmR and FoR estimated from final filtered
                           # data will be used to calculate ChlF parameters
                           FmR.data = FmR.df %>% subset(var=='Filtered data'),
                           save.file = T,save.path = 'saved data')
names(seas.para)
```

## 3.3 visualize diurnal and seasonal parameters

plot and save seasonal parameter figure

```{r fig.height=20,fig.width=15}
plotSeasonPara(season.param = seas.para,save.path = 'figures/figure_seasonal_parameters',fieldnote = F)
```

plot and save diurnal parameter figures

```{r fig.height=10,fig.width=15}
# plot and save rate constant parameter figure to local folder
# PQ,NPQ,NPQr
plotDiurnRateConstant(diurnal.para = diurn.para,
                      fieldnote = T,fieldnote.data = fieldnote_2014_2015,
                      save.path = 'figures/figure_diurnal_parameters')
# plot and save quenching parameter figure to local folder
#qLT and qLr
plotQuench(diurnal.para = diurn.para,
           fieldnote = T,fieldnote.data = fieldnote_2014_2015,
           save.path = 'figures/figure_diurnal_parameters')


# plot and save quantum yield parameter figure to local folder
# Phi_PQ(YII),Phi_NPQ,Phi_fD

plotYield(diurnal.para = diurn.para,
          fieldnote = T,fieldnote.data = fieldnote_2014_2015,
          save.path = 'figures/figure_diurnal_parameters')

# plot and save NPQ yield parameter figure to local folder
# Phi_NPQ,Phi_NPQr,Phi_NPQs

plotNPQYield(diurnal.para = diurn.para,
             fieldnote = T,fieldnote.data = fieldnote_2014_2015,
             save.path = 'figures/figure_diurnal_parameters')
```

# 4. try data from 2016-2017

## 4.1 read and organize MONIPAM data: readPAM function

```{r}
PAM2016.2017<-
  readPAM(source.path=system.file("extdata/2016-2017/",package = "LongTermPAM"),
               pam.pattern = '.PAM|.pam',
               save.path = getwd(),# it is better to modify this to your local path
               site.lat =61.84562, # hyytiala latitude
               site.lon = 24.29077, # hyytiala longitude
               local.tz = 'EET', # time zone of Finland
               tz.summer =  3,
               tz.winter = 2,
               measure.time = 'winter' # In Hyytiala forest station, we always use winter time
               )
```

## 4.2 correct head and/or tree_num information if needed

```{r}
#read the head information into R, remember your saved-data path
head.timerange<-read.csv("preprocesPAM_head_timerange_2016_2017.dat",header = T,sep = ';')
#print this head and tree information
head.timerange
```

In this example, we can see that there are (1) two unknow tree_num:
Unknow1 and Unknow2 and they were only measured two days in total and
(2) head 25 with tree_num Pine2LOW which actually only recorded few data
after checking the data Thus, we do not need these three trees and can
remove then from the preprocessed data (not from this head info file):
preprocesPAM_2016_2017.dat

```{r}
# read preprocesPAM_2014_2015.dat into R using fread from data.table package
preproces.20162017<-fread('preprocesPAM_2016_2017.dat')

# arrange the preproces.20162017 data to correct data format using formatPAMdata function from this package
preproces.20162017<-formatPAMdata(preproces.20162017)

# correct tree_num
preproces.20162017<-
  preproces.20162017 %>% 
  subset(tree_num!='Unknow1'&tree_num!='Unknow2'&tree_num!='Pine2LOW') %>% 
  droplevels() %>% 
  mutate(head_tree=paste0(head,'_',tree_num))

# check tree_num
levels(preproces.20162017$tree_num) # we can see now 'Unknow1', 'Unknow2' and 'Pine2LOW' are removed
# check data
head(preproces.20162017)
```

## 4.3 remove first few days data

**Here we remove the data before 2016-11-05 12:00 after checking the
data using WinControl software**

```{r}
preproces.20162017<-preproces.20162017 %>% subset(datetime>ymd_hms('2016-11-05 12:00:00'))
# check the data now
head(preproces.20162017)
```

## 4.4 correct F if needed

Temperature dependency of ChlF correction. The correction parameters
were given default value but can also be adjusted. Details check helps
of correctF() function

```{r}
preproces.20162017.correctF<-correctF(preproces.20162017)
```

## 4.5 save the modified/corrected file

```{r}
# write.table function from R
# I call it corrected data, because usually we need correct head and tree info for our Hyytiala data
write.table(preproces.20162017.correctF,file='corrected2016-2017.dat',row.names = F,sep = ';')
```

## 4.6. data cleaning

define work directory to save the data and figures

```{r}
# This is only an example, you can decide where the cleaned/filtered data and visualized figure will be saved 

# Function to create a directory if it doesn't exist
create_dir_if_not_exists <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)  # recursive = TRUE ensures that any necessary parent directories are created
  }
}

# Create a folder for saving all the data
create_dir_if_not_exists('saved data')

# Create a new folder to save data visualization figures
create_dir_if_not_exists('figures')

# Create subfolders to save different data visualization figures
create_dir_if_not_exists('figures/figure_raw_data')
create_dir_if_not_exists('figures/figure_filter_step')
create_dir_if_not_exists('figures/figure_diurnal_parameters')
create_dir_if_not_exists('figures/figure_seasonal_parameters')
```

```{r}
# read organized and corrected MONIPAM data for data cleaning
# fread function from data.table package
flag0<-fread("corrected2016-2017.dat",stringsAsFactors = T)

# This is only an example, you can decide where the cleaned/filtered data and visualized figure will be saved 

# there is no fieldnote data for 2016-2017
#define folder for saving the data and plots
save.fitlerFigure.path<-'figures/figure_filter_step'
save.filterData.path<-'saved data'
save.seasonalFigure.path<-'figures/figure_seasonal_parameters'


# first, let's see how fvfm from original data varies across whole observing season
# calculate FvFm using FindFvFm and check data by plotting fvfm data

flag0.fvfm<-  FindFvFm(PAM.data=flag0,save.path = save.filterData.path,
                       save.title = 'Flag 0',save.file=T)
# plot and save to local folder
plotFvFm(flag0.fvfm, plot.title = 'Flag 0 fvfm', 
         save.path=save.seasonalFigure.path,fieldnote = F)
# visulize original data  using plotRawData() function 
plotRawData(flag0, fieldnote = F,plot.title = 'with sunlight time',save.path = save.rawdataFigure.path)

```

```{r}
# cleaning data steps 1 to 6**
flag1.lowF<-filter1.lowF(PAM.data=flag0,save.path=save.filterData.path,
                     save.file=T)
flag1.fvfm<-FindFvFm(flag1.lowF, save.filterData.path,'Flag 1',save.file=T)
plotFvFm(flag1.fvfm,plot.title = 'Flag 1 fvfm',save.path = save.seasonalFigure.path,
         fieldnote = F)
plotCheckfilter(newdata=flag1.lowF,rawdata=flag0,fieldnote = F,
                save.path=save.fitlerFigure.path)

# adjust f2.fm from 0.01 to 0.1 by a 0.01 interval
# f2.fmYII = 3 usually is a good setting, you can just try to adjust f2.fm
flag2.night<-filter2.night(flag1.lowF,f2.fm=0.05,f2.fmYII=3,
                           save.filterData.path,save.file = T)
flag2.fvfm<-FindFvFm(flag2.night, save.filterData.path,'Flag 2',save.file=T)
plotFvFm(flag2.fvfm,plot.title = 'Flag 2 fvfm',fieldnote = F,
         save.path= save.seasonalFigure.path)
plotCheckfilter(newdata=flag2.night,rawdata=flag0,fieldnote = F,
                save.path=save.fitlerFigure.path)

# adjust f3.YII from 0.01 to 0.1 by a 0.01 interval
# adjust f3.fm from 0.05 to 0.3 by a 0.05 interval
# f3.fmYII = 3 usually is a good setting, you can just try to adjust f3.YII and f3.fm
flag3.day<-
   filter3.day(flag2.night,f3.YII=0.02,f3.fm=0.1,f3.fmYII = 3,
                save.path =save.filterData.path,save.file = T)
flag3.fvfm<-
  FindFvFm(flag3.day, save.filterData.path,'Flag 3',save.file=T)

plotFvFm(flag3.fvfm,plot.title = 'Flag 3 fvfm',fieldnote = F,
        save.path =  save.seasonalFigure.path)

plotCheckfilter(newdata=flag3.day,rawdata=flag0,fieldnote = F,
                save.path=save.fitlerFigure.path)

# adjust f4.fm from 0.05 to 0.3 by a 0.05 interval
# f4.fmYII = 3 usually is a good setting, you can just try to adjust f4.fm
flag4.FvFm<-
   filter4.FvFm(flag3.day,save.file = T,f4.fm=0.15,f4.fmYII=3,
                    save.path = save.filterData.path)
flag4.fvfm<-
  FindFvFm(flag4.FvFm,save.filterData.path,'Flag 4',save.file=T)

plotFvFm(flag4.fvfm,plot.title = 'Flag 4 fvfm',fieldnote = F,
        save.path= save.seasonalFigure.path)
plotCheckfilter(newdata=flag4.FvFm,rawdata=flag0,fieldnote = F,
                save.path=save.fitlerFigure.path)


# adjust f5.fm from 0.1 to 0.3 by a 0.05 interval
# f5.fmYII = 3 usually is a good setting, you can just try to adjust f5.fm
flag5.expand<-filter5.expand(flag4.FvFm,save.file = T,
                           save.path = save.filterData.path,
                            f5.fm = 0.2,f5.fmYII = 3)
flag5.fvfm<-
  FindFvFm(flag5.expand,save.filterData.path,'Flag 5',save.file=T)


plotFvFm(flag5.fvfm,plot.title = 'Flag 5 fvfm',fieldnote = F,
         save.path=save.seasonalFigure.path)
plotCheckfilter(newdata=flag5.expand,rawdata=flag0,fieldnote = F,
                save.path=save.fitlerFigure.path)

# adjust expand.time  according your data recording interval, 60 mins here for this example include 2 measurement points

flag6.adjacent<-filter6.adjacent(flag5.expand,save.file = T,
                          save.path = save.filterData.path,
                          expand.time = 60)
flag6.fvfm<-
  FindFvFm(flag6.adjacent,save.filterData.path,'Flag 6',save.file=T)

plotFvFm(flag6.fvfm,plot.title = 'Flag 6 fvfm',fieldnote = F,
         save.path=save.seasonalFigure.path)
plotCheckfilter(newdata=flag6.adjacent,rawdata=flag0,fieldnote = F,
                save.path=save.fitlerFigure.path)
```

## 4.7. calculate other paramters

```{r}
create_dir_if_not_exists('figures/figure_FmR_FoR')
save.path='figures/figure_FmR_FoR'
```

### 1. estimate FmR and FoR using ChlFRef function

```{r fig.height=6,fig.width=20}
FmR.df<-ChlFRef(raw.fvfm = flag0.fvfm,filter.fvfm = flag6.fvfm,
                FvFmR = 0.83,save.file = T,save.path = 'figures/figure_FmR_FoR')
FmR.df
```

### 2. estimate diurnal parameters using diurnalParams function

This function will estimate diurnal parameters,including rate constant
parameters: PQ, NPQ,NPQr; quenching parameters: qLT,qLr, and quantum
yield parameters: Phi_NPQ, Phi_NPQr,Phi_NPQs, Phi_fD. Please see
calculation method and parameter description in Porcar-Castell et al.
2011 and Zhang et al. XX

```{r}
diurn.para<-diurnalParams(filtered.data = flag6.adjacent,
                          filtered.fvfm = flag6.fvfm,
                          # only the FmR and FoR estimated from final filtered
                          # data will be used to calculate ChlF parameters
                          FmR.data = FmR.df %>% subset(var=='Filtered data'),
                          save.file = T,save.path = 'saved data')

names(diurn.para)
```

### 3. estimate seasonal parameters using seasonalParams function

This function will estimate diurnal parameters,including rate constant
parameters: PQ, NPQ,NPQr; quenching parameters: qLT,qLr, and quantum
yield parameters: Phi_NPQ, Phi_NPQr,Phi_NPQs, Phi_fD. Please see
calculation method and parameter description in Porcar-Castell et al.
2011 and Zhang et al. XX

```{r}
seas.para<-seasonalParams(filtered.fvfm = flag6.fvfm,
                           # only the FmR and FoR estimated from final filtered
                           # data will be used to calculate ChlF parameters
                           FmR.data = FmR.df %>% subset(var=='Filtered data'),
                           save.file = T,save.path = 'saved data')

names(seas.para)
```

### 4. visualize diurnal and seasonal parameters

plot and save seasonal parameter figure

```{r fig.height=20,fig.width=15}
plotSeasonPara(season.param = seas.para,
               save.path = 'figures/figure_seasonal_parameters', 
               fieldnote = F)
```

plot and save diurnal parameter figures

```{r fig.height=10,fig.width=15}
# plot and save rate constant parameter figure to local folder
# PQ,NPQ,NPQr
plotDiurnRateConstant(diurnal.para = diurn.para,
                      fieldnote = F,
                      save.path = 'figures/figure_diurnal_parameters')
# plot and save quenching parameter figure to local folder
#qLT and qLr
plotQuench(diurnal.para = diurn.para,
           fieldnote = F,
           save.path = 'figures/figure_diurnal_parameters')


# plot and save quantum yield parameter figure to local folder
# Phi_PQ(Yield),Phi_NPQ,Phi_fD

plotYield(diurnal.para = diurn.para,
          fieldnote = F,
          save.path = 'figures/figure_diurnal_parameters')

# plot and save NPQ yield parameter figure to local folder
# Phi_NPQ,Phi_NPQr,Phi_NPQs

plotNPQYield(diurnal.para = diurn.para,
             fieldnote = F,
             save.path = 'figures/figure_diurnal_parameters')

```
