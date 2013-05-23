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
read.smsbar <- function(file, return_all = FALSE, us_numbers = TRUE) {

  ## read in XML file 
  sms_data <- xmlInternalTreeParse("/home/eiverson/src/sms/data/test2.xml")
  sms_list <- xmlToList(sms_data)
  df <- as.data.frame(do.call(rbind, sms_list[-length(sms_list)]))

  ## fix up dates 
  df$date_time_rcvd <- as.POSIXct(as.numeric(substr(df$date, 1 , 10)),
                             origin = "1970-01-01",
                             tz = "GMT")

  df$date_time_sent <- as.POSIXct(as.numeric(substr(df$date_sent, 1 , 10)),
                                  origin = "1970-01-01",
                                  tz = "GMT")

  ## derive some other helpful variables 
  df$type <- factor(df$type, levels = c("1", "2", "3"),
                    labels = c("Incoming", "Outgoing", "Other"))
  df$type[is.na(df$type)] <- "Other"
  df$body <- as.character(df$body)
  df$length <- nchar(df$body)

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
    df$date <- NULL
    df$readable_date <- NULL
    df$date_sent <- NULL
  }

  ## canonicolize the address field for US phone numbers
  if(us_numbers)
    df$address <- as.factor(gsub("^\\+?1", "", df$address))
    
  df
}

sms_plot <- function(data, inc_unk = TRUE) {
  data <- data.table(data)
  setkeyv(data, c("contact_name", "date_time_rcvd"))
  daily_data <- data[, list(date = unique(as.Date(date_time_rcvd)),
                            n_texts = cumsum(table(as.Date(date_time_rcvd)))),
                     by = "contact_name"]
  ggplot(daily_data, aes(x = date, y = n_texts, colour = contact_name)) +
    geom_line() + theme_bw()
}
