#' @title Read a TOA5 style logger file
#' @description Reads a logger text file into a dataframe. The Timestamp variable is renamed to 'DateTime', and converted to POSIXct. 
#' @param filename The file to be read
#' @param nlabelline How many lines at the top of the file contain labels? Will be automated in a future version.
#' @param dateTimeMethod Either 'lubridate' (the default) or 'POSIXct'. Extra arguments to either can be provided via \dots .
#' @param posixctFormat If dateTimeMethod is 'POSIXct', the format string for the datetime.
#' @param addFileName If TRUE (the default), adds a new variable 'Source', the filename from which the data was read.
#' @param rmDupDateTime Logical, whether to remove duplicated datetimes from the data (default is FALSE).
#' @param \dots Extra arguments to the datetime conversion method.
#' @export
#' @return A dataframe
readTOA5 <- function(filename, nlabelline=4, 
                     dateTimeMethod=c("lubridate","POSIXct","fasttime"), 
                     posixctFormat="%Y-%m-%d %H:%M",
                     addFileName=TRUE, rmDupDateTime=FALSE, ...){
  
  dateTimeMethod <- match.arg(dateTimeMethod)
  
  if(dateTimeMethod == "fasttime"){
    r <- require(fasttime)
    if(!r)
      stop("Install fasttime package first!:\ninstall.packages('fasttime', repos='http://www.rforge.net/')")
  }
  
  getData <- function(fn){
    h <- readLines(fn, n=nlabelline)
    h <- gsub(",([0-9])", "\\1", h)
    dat <- read.csv(fn, skip=nlabelline, header=FALSE, na.strings="NAN")
    colnames <- gsub("\"", "", strsplit(paste(h[2], collapse = ""), ",")[[1]])
    names(dat) <- make.names(colnames)
    names(dat)[1] <- "DateTime"
    return(dat)
  }
  dat <- try(getData(filename))
  if(inherits(dat,"try-error")){
    message("Could not read file ", filename, ". It is probably not TOA5 or otherwise corrupted.\n Returning NULL.")
    return(NULL)
  }
  
  if(dateTimeMethod=="lubridate")
    dat$DateTime <- ymd_hms(as.character(dat$DateTime), tz="UTC")
  
  if(dateTimeMethod=="POSIXct")
    dat$DateTime <- as.POSIXct(dat$DateTime, format=posixctFormat, tz="UTC")
  
  if(dateTimeMethod=="fasttime")
    dat$DateTime <- fastPOSIXct(dat$DateTime, tz="UTC")
  
  dat$Date <- as.Date(dat$DateTime)
  
  if(rmDupDateTime)dat <- dat[!duplicated(dat$DateTime),]
  
  if(addFileName)dat$Source <- basename(filename)
  
  return(dat)
}
