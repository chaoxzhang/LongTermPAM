% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FindFVFM.R
\name{FindFVFM}
\alias{FindFVFM}
\title{Retrieve FV/FM data from MONI-PAM data}
\usage{
FindFVFM(PAM.data, save.path, save.title, save.file)
}
\arguments{
\item{PAM.data}{a combined organized MONI-PAM data which is generated from \link{readPAM} function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as filter1.NA, filter2.night and so on.}

\item{save.path}{local folder for saving your output data}

\item{save.title}{any text that will be used as the filename for saving the file}

\item{save.file}{If this argument is set as TRUE, the returned file will be saved to local folder, if False, the file will not be saved into local folder}
}
\value{
\link{FindFVFM} will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a '.dat' file.
}
\description{
This function will get maximum values of quantum yield of photosystem II (maximum Y(II)) for each observing night (highest Y(II) between sunset of Day i and sunrise of Day i+1) for each MONI-head. We use these maximum YII to represent FV/FM, because during the night time, the PAR is close to 0 and almost all photosynthetic reaction center should be open for hours.
}
