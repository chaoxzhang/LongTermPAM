#' @title fieldnote 2014-2015
#'
#' @description This fieldnote dataset describes the history notes during the MONIPAM observation period from 2014 to 2015. Note that in the package we also have data from 2016 to 2017, but we do not have a fieldnote file for that observing season.
#'
#' @format A \code{data.table} with 11 columns and 65 rows, which are:
#' \describe{
#'  \item{SEASON}{indicates the observation period from 2014 to 2015}
#'  \item{date}{the date for corresponding event occurred, e.g., replace to a new set of needles}
#'  \item{time}{the time for corresponding event occurred}
#'  \item{head}{MONI-PAM head number}
#'  \item{tree_num}{measured plant reference code. Here, T means tree, TOP and LOW means top and low of the canopy respectively,and value means tree number}
#'  \item{remark.plot}{this is the short name of the event, and will be used in plot visualization}
#'  \item{remark}{this is the full name of the event and should be clearly clarify what has been happened}
#'  \item{manager}{the researcher who found and recorded the event}
#'  \item{datetime}{combined date and time columns for later data visualization purpose}
#'  \item{plot.group}{for data visualization purpose, plot.group is defined  based on every month days 1-10 as group a, 11-20 as b, and others as c}
#'  \item{head_tree}{combined head and tree_num column for later data visualization purpose}
#' }
"fieldnote_2014_2015"
