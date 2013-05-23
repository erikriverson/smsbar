#' sms_bar
#'
#' @name smsbar
#' @docType package
NULL


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param file 
##' @return 
##' @author 
read.smsbar <- function(file) {
  sms_data <- xmlParse(file)
  sms_root <- xmlRoot(sms_data)

  agg <- vector(mode = "list", length = xmlSize(sms_root))
  for(i in 1:xmlSize(sms_root))
    agg[[i]] <- xmlAttrs(sms_root[[i]])
  str(df <- as.data.frame(do.call(rbind, agg)))

  ## remove unhelpful columns from the data.frame 
  df$protocol <- NULL
  df$read <- NULL
  df$toa <- NULL
  df$subject <- NULL
  df$sc_toa <- NULL
  df$service_center <- NULL
  df$status <- NULL
  df$locked <- NULL

  ## get rid of types 3 and 5, MMS/pics? 
  df <- subset(df, !type %in% c("3", "5"))
  df$type <- factor(df$type, levels = c("1", "2"),
                    labels = c("Incoming", "Outgoing"))

  df$body <- as.character(df$body)
  df$text_length <- nchar(df$body)

  df$text_date <- as.POSIXct(as.numeric(substr(df$date, 1 , 10)),
                             origin = "1970-01-01",
                             tz = "GMT")
  df$Day <- as.Date(df$text_date, format = "%m/%d/%Y")


  df
}
