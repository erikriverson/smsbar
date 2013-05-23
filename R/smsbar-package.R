#' smsbar
#'
#' @name smsbar
#' @docType package
NULL


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Read an SMS Backup & Restore XML file
##' @param file path name to XML file created by SMS Backup & Restore
##' @return data.frame of SMS text messages, with one row per message
##' @author Erik Iverson
read.smsbar <- function(file, return_all = FALSE) {
  sms_data <- xmlParse(file)
  sms_root <- xmlRoot(sms_data)

  agg <- vector(mode = "list", length = xmlSize(sms_root))
  for(i in 1:xmlSize(sms_root))
    agg[[i]] <- xmlAttrs(sms_root[[i]])
  str(df <- as.data.frame(do.call(rbind, agg)))

  ## remove unhelpful columns from the data.frame
  if(!return_all) {
    df$protocol <- NULL
    df$read <- NULL
    df$toa <- NULL
    df$subject <- NULL
    df$sc_toa <- NULL
    df$service_center <- NULL
    df$status <- NULL
    df$locked <- NULL
  }

  df$type <- factor(df$type, levels = c("1", "2"),
                    labels = c("Incoming", "Outgoing"))
  df$body <- as.character(df$body)
  df$length <- nchar(df$body)

  df$text_date <- as.POSIXct(as.numeric(substr(df$date, 1 , 10)),
                             origin = "1970-01-01",
                             tz = "GMT")
    
  df
}
