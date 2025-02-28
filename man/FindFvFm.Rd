% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FindFvFm.R
\name{FindFvFm}
\alias{FindFvFm}
\title{Retrieve Fv/Fm data from MONI-PAM data}
\usage{
FindFvFm(PAM.data,save.path,save.title, save.file)
}
\arguments{
\item{PAM.data}{a combined organized MONI-PAM data which is generated from \link{readPAM} function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as filter1.NA, filter2.night and so on.}

\item{save.path}{local folder for saving your output data}

\item{save.title}{any text that will be used as the saved file name}

\item{save.file}{If this argument is set as TRUE, the returned file will be saved to local folder, if False, the file will not be saved into local folder}
}
\value{
\link{FindFvFm} will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a '.dat' file.
}
\description{
This function will get maximum values of quantum YII of photosystem II (maximum YII) for each observing night (highest YII between sunset of Day i and sunrise of Day i+1) for each MONI-head. We use these maximum YII to represent FV/FM because during the night time, the PAR is close to 0 and almost all photosynthetic reaction center should be open for hours.  In principle, the maximum YII should be observed during the dark period when the PAR is close to 0 and almost all photosynthetic reaction centers should be closed for hours. This is the reason that we can use the maximum YII from nighttime to represent FV/FM in continuous MONI-PAM measurement
}
